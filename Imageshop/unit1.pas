(******************************************************************************)
(* ImageShop                                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.05                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Edit, Mix, and Modify images via drag & Drop, write own      *)
(*               Color manipulation algorithms and see workpipes in action to *)
(*               adjust images                                                *)
(*                                                                            *)
(* Inspired by: https://github.com/sysrpl/Codebot.ImageShop/tree/master       *)
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
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Roll Horizontal, Roll Vertical, Rotate                *)
(*                      Save / Load Scene                                     *)
(*               0.03 - die Sliderpos und Images mit Speichern                *)
(*                      Ettliche neue Effekte                                 *)
(*                      Bugfixes im Eventer                                   *)
(*                      Besseres Zeichnen der Connectoren                     *)
(*                      Save Result                                           *)
(*               0.04 - Custom Operatoren / Blender (leider recht langsam)    *)
(*               0.05 - Zoom                                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, OpenGLContext, dglopengl, unodes, Types, uopengl_graphikengine,
  uoperations;

Const
  GridSize = 5;
  Version = 005;

Type

  TConnectorType = (ctnone, ctin1, ctin2, ctout);

  TOperator = Record
    Name: String;
    Element: TNodeClass;
  End;

  TConnectorStartRecord = Record
    Position: TPoint;
    Element: TNode;
    Connector: TConnectorType; // 0 = nicht initialisiert, 1 = in1, 2 = in2, 4 = out
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    ImageList1: TImageList;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControl1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox1Resize(Sender: TObject);
  private
    fResultingImage: TImageDest;
    fOperators: Array Of TOperator;
    fNodes: Array Of TNode;
    fSelectedElement: TNode;
    fMouseDownDelta: TPoint;
    fConnector: TConnectorStartRecord;
    backtex: integer;
    fFilename: String;
    fdefcaption: String;
    fchanged: Boolean;
    fZoom: Single;
    Procedure Go2d();
    Procedure Exit2d();
    Function GetSelectedElement(x, y: integer): TNode;
    Procedure OnNodeCloseButtonClick(Sender: TObject);
    Procedure OnOutConnectorClick(Sender: TObject);
    Procedure OnInConnectorClick(Sender: TObject);
    Procedure OnInConnector2Click(Sender: TObject);
    Procedure AddOperatorToList(aName: String; Op: TNodeClass);
    Procedure OnHint(Sender: TObject; aHint: String);
    Procedure SetCaption;
    Procedure SetChanged;
    Procedure SaveScene(Const Filename: String);
    Procedure LoadScene(Const Filename: String);
    Procedure Clear;
    Procedure SetEvents(Var Element: TNode);
  public
    Procedure CreateListBoxMenu; // Erzeugt das Menü für die Listbox
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

Uses LCLType, math, inifiles, Unit2, LazFileUtils;

{$R *.lfm}

Var
  ini: Tinifile;

Function PointInRect(P: TPoint; r: Trect): Boolean;
Begin
  result :=
    (p.x >= min(r.Left, r.Right)) And
    (p.x <= max(r.Left, r.Right)) And
    (p.y >= min(r.Top, r.Bottom)) And
    (p.y <= max(r.Top, r.Bottom));
End;

Procedure TForm1.Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 0, 480, -1, 1); // Set Up An Ortho Screen
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure TForm1.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Function TForm1.GetSelectedElement(x, y: integer): TNode;
Var
  i: integer;
  p: Tpoint;
Begin
  result := Nil;
  p := point(x, y);
  For i := 0 To high(fNodes) Do Begin
    If PointInRect(p, fNodes[i].ClientRect) Then Begin
      result := fNodes[i];
      exit;
    End;
  End;
End;

Procedure TForm1.OnNodeCloseButtonClick(Sender: TObject);
Var
  i, j: Integer;
Begin
  // Löschen einer evtl in Connection
  For i := 0 To high(fNodes) Do Begin
    If fNodes[i].InNode = Sender Then fNodes[i].InNode := Nil;
    If fNodes[i].In2Node = Sender Then fNodes[i].In2Node := Nil;
  End;
  If fResultingImage.InNode = sender Then fResultingImage.InNode := Nil;
  // Suchen und Löschen des Elements in der Globalen Node Liste
  For i := 0 To high(fNodes) Do Begin
    If fNodes[i] = Sender Then Begin
      SetChanged;
      fNodes[i].free;
      For j := i To high(fNodes) - 1 Do Begin
        fNodes[j] := fNodes[j + 1];
      End;
      setlength(fNodes, high(fNodes));
      PaintBox1.Invalidate;
      OpenGLControl1Paint(Nil);
    End;
  End;
End;

Procedure TForm1.OnOutConnectorClick(Sender: TObject);
Begin
  If fConnector.Connector = ctnone Then Begin
    fConnector.Element := Sender As TNode;
    fConnector.Position := point(
      fConnector.Element.Left + fConnector.Element.Width + 10,
      fConnector.Element.Top + fConnector.Element.Height Div 2
      );
    fConnector.Connector := ctout;
  End
  Else Begin
    If (sender <> fConnector.Element) And (fConnector.Connector <> ctout) Then Begin
      SetChanged;
      If fConnector.Connector = ctin1 Then Begin
        If fConnector.Element.InNode = sender As TNode Then Begin
          fConnector.Element.InNode := Nil;
        End
        Else Begin
          fConnector.Element.InNode := sender As TNode;
          If Not fResultingImage.CheckEndlesRecursion(length(fNodes)) Then Begin
            ShowMessage('Inserting this connection would give a endless recursion. Please remove the circel in your design first.');
            fConnector.Element.InNode := Nil;
          End;
        End;
      End
      Else Begin
        If fConnector.Element.In2Node = sender As TNode Then Begin
          fConnector.Element.In2Node := Nil;
        End
        Else Begin
          fConnector.Element.In2Node := sender As TNode;
          If Not fResultingImage.CheckEndlesRecursion(length(fNodes)) Then Begin
            ShowMessage('Inserting this connection would give a endless recursion. Please remove the circel in your design first.');
            fConnector.Element.In2Node := Nil;
          End;
        End;
      End;
      PaintBox1.Invalidate;
      OpenGLControl1Paint(Nil);
      fConnector.Connector := ctnone;
    End;
  End;
End;

Procedure TForm1.OnInConnectorClick(Sender: TObject);
Begin
  If fConnector.Connector = ctnone Then Begin
    fConnector.Element := Sender As TNode;
    If fConnector.Element Is TTwoInputNode Then Begin
      fConnector.Position := point(
        fConnector.Element.Left - 10,
        fConnector.Element.Top + fConnector.Element.Height Div 4
        );
    End
    Else Begin
      fConnector.Position := point(
        fConnector.Element.Left - 10,
        fConnector.Element.Top + fConnector.Element.Height Div 2
        );
    End;
    fConnector.Connector := ctin1;
  End
  Else Begin
    If (sender <> fConnector.Element) And (fConnector.Connector = ctout) Then Begin
      SetChanged;
      If (sender As TNode).InNode = fConnector.Element Then Begin
        (sender As TNode).InNode := Nil;
      End
      Else Begin
        (sender As TNode).InNode := fConnector.Element;
        If Not fResultingImage.CheckEndlesRecursion(length(fNodes)) Then Begin
          ShowMessage('Inserting this connection would give a endless recursion. Please remove the circel in your design first.');
          (sender As TNode).InNode := Nil;
        End;
      End;
      PaintBox1.Invalidate;
      OpenGLControl1Paint(Nil);
      fConnector.Connector := ctnone;
    End;
  End
End;

Procedure TForm1.OnInConnector2Click(Sender: TObject);
Begin
  If fConnector.Connector = ctnone Then Begin
    fConnector.Element := Sender As TNode;
    fConnector.Position := point(
      fConnector.Element.Left - 10,
      fConnector.Element.Top + (3 * fConnector.Element.Height) Div 4
      );
    fConnector.Connector := ctin2;
  End
  Else Begin
    If (sender <> fConnector.Element) And (fConnector.Connector = ctout) Then Begin
      SetChanged;
      If (sender As TNode).In2Node = fConnector.Element Then Begin
        (sender As TNode).In2Node := Nil;
      End
      Else Begin
        (sender As TNode).In2Node := fConnector.Element;
        If Not fResultingImage.CheckEndlesRecursion(length(fNodes)) Then Begin
          ShowMessage('Inserting this connection would give a endless recursion. Please remove the circel in your design first.');
          (sender As TNode).In2Node := Nil;
        End;
      End;
      PaintBox1.Invalidate;
      OpenGLControl1Paint(Nil);
      fConnector.Connector := ctnone;
    End;
  End
End;

Procedure TForm1.AddOperatorToList(aName: String; Op: TNodeClass);
Begin
  setlength(fOperators, high(fOperators) + 2);
  fOperators[high(fOperators)].Name := aName;
  fOperators[high(fOperators)].Element := op;
  ListBox1.Items.Add('e' + aName);
End;

Procedure TForm1.OnHint(Sender: TObject; aHint: String);
Var
  w, h: integer;
Begin
  If aHint <> '' Then Begin
    PaintBox1.Canvas.Pen.Color := clblack;
    PaintBox1.Canvas.Brush.Color := clSilver;
    PaintBox1.Canvas.Brush.Style := bsSolid;
    w := PaintBox1.Canvas.TextWidth(aHint);
    h := PaintBox1.Canvas.TextHeight(aHint);
    PaintBox1.Canvas.Rectangle(
      PaintBox1.Width - w - 4,
      PaintBox1.Height - h - 4,
      PaintBox1.Width,
      PaintBox1.Height
      );
    PaintBox1.Canvas.TextOut(
      PaintBox1.Width - w - 2,
      PaintBox1.Height - h - 2,
      aHint
      );
  End;
End;

Procedure TForm1.SetCaption;
Begin
  Caption := fdefcaption;
  If fFilename <> '' Then Begin
    caption := Caption + ' [' + ExtractFileName(fFilename) + ']';
    If fchanged Then Begin
      Caption := Caption + '*';
    End;
  End
  Else Begin
    If fchanged Then Begin
      Caption := Caption + ' *';
    End;
  End;
End;

Procedure TForm1.SetChanged;
Begin
  fchanged := true;
  SetCaption;
End;

Procedure TForm1.SaveScene(Const Filename: String);
Var
  f: TFileStream;
  x, y, i, j: integer;
  s: Single;
Begin
  fFilename := Filename;
  fchanged := false;
  SetCaption;
  f := TFileStream.Create(fFilename, fmCreate Or fmOpenWrite);
  // Anwendungsversion
  i := Version;
  f.Write(i, SizeOf(i));
  // Zoomstufe
  s := fZoom;
  f.Write(s, SizeOf(s));
  // Anzahl der Elmente
  i := Length(fNodes);
  f.Write(i, SizeOf(i));
  // Im Ersten Durchlauf müssen alle Captions Gespeichert werden, damit die Elemente wieder hergestellt werden können
  For i := 0 To high(fNodes) Do Begin
    f.WriteAnsiString(fNodes[i].ClassName);
    x := fNodes[i].Left;
    f.Write(x, SizeOf(x));
    y := fNodes[i].Top;
    f.Write(y, SizeOf(y));
    // Den Relativen Pfad eines Bildes Speichern
    If (fNodes[i] Is TImageSource) Then Begin
      f.WriteAnsiString((fNodes[i] As TImageSource).Filename);
    End;
    If (fNodes[i] Is TOneInputSliderNode) Then Begin
      s := (fNodes[i] As TOneInputSliderNode).SliderPosition;
      f.Write(s, sizeof(s));
    End;
    If (fNodes[i] Is TTwoInputSliderNode) Then Begin
      s := (fNodes[i] As TTwoInputSliderNode).SliderPosition;
      f.Write(s, sizeof(s));
    End;
    If (fNodes[i] Is TCustomOperator) Then Begin
      f.WriteAnsiString(TCustomOperator(fNodes[i]).Filename);
    End;
    If (fNodes[i] Is TCustomBlender) Then Begin
      f.WriteAnsiString(TCustomBlender(fNodes[i]).Filename);
    End;
  End;
  // Im Zweiten Durchlauf kommen dann die Kanten, da die OneInputNodes den in2 nie definieren, geht das hier so einfach *g*
  For i := 0 To high(fNodes) Do Begin
    y := -1;
    For j := 0 To high(fNodes) Do Begin
      If fNodes[j] = fNodes[i].InNode Then Begin
        y := j;
        break;
      End;
    End;
    f.Write(y, SizeOf(y));
    y := -1;
    For j := 0 To high(fNodes) Do Begin
      If fNodes[j] = fNodes[i].In2Node Then Begin
        y := j;
        break;
      End;
    End;
    f.Write(y, SizeOf(y));
  End;
  // Das Finale Bild hat ja auch einen Eingang
  y := -1;
  For i := 0 To high(fNodes) Do Begin
    If fResultingImage.InNode = fNodes[i] Then Begin
      y := i;
      break;
    End;
  End;
  f.Write(y, SizeOf(y));
  f.free;
End;

Procedure TForm1.LoadScene(Const Filename: String);
Var
  j, ver, i, x, y: integer;
  f: TFileStream;
  s: String;
  si: Single;
Begin
  Clear;
  fZoom := 1;
  fFilename := Filename;
  f := TFileStream.Create(Filename, fmOpenRead);
  // Versionsnummer
  ver := 0;
  f.Read(ver, SizeOf(ver));
  If ver >= 5 Then Begin
    si := 1;
    f.Read(si, SizeOf(si));
    fZoom := si;
  End;
  // Anzahl
  i := 0;
  f.Read(i, SizeOf(i));
  setlength(fNodes, i);
  // Alle Elemente
  For i := 0 To high(fNodes) Do Begin
    s := f.ReadAnsiString;
    x := 0;
    f.Read(x, SizeOf(x));
    y := 0;
    f.Read(y, SizeOf(y));
    For j := 0 To high(fOperators) Do Begin
      If fOperators[j].Element.ClassName = s Then Begin
        fNodes[i] := fOperators[j].Element.Create(PaintBox1, x, y);
        SetEvents(fNodes[i]);
        break;
      End;
    End;
    If ver >= 3 Then Begin // Ab Version 3 können die Sliderpositionen und Bilder mit gespeichert
      // Den Relativen Pfad eines Bildes Speichern
      If (fNodes[i] Is TImageSource) Then Begin
        (fNodes[i] As TImageSource).Filename := f.ReadAnsiString();
      End;
      If (fNodes[i] Is TOneInputSliderNode) Then Begin
        si := 0;
        f.Read(si, sizeof(si));
        (fNodes[i] As TOneInputSliderNode).SliderPosition := si;
      End;
      If (fNodes[i] Is TTwoInputSliderNode) Then Begin
        si := 0;
        f.Read(si, sizeof(si));
        (fNodes[i] As TTwoInputSliderNode).SliderPosition := si;
      End;
    End;
    If ver >= 4 Then Begin
      If (fNodes[i] Is TCustomOperator) Then Begin
        s := f.ReadAnsiString;
        TCustomOperator(fNodes[i]).LoadSourceCode(s);
      End;
      If (fNodes[i] Is TCustomBlender) Then Begin
        s := f.ReadAnsiString;
        TCustomBlender(fNodes[i]).LoadSourceCode(s);
      End;
    End;
  End;
  // Die Kanten
  For i := 0 To high(fNodes) Do Begin
    j := -1;
    f.Read(j, SizeOf(j));
    If j <> -1 Then Begin
      fNodes[i].InNode := fNodes[j];
    End;
    j := -1;
    f.Read(j, SizeOf(j));
    If j <> -1 Then Begin
      fNodes[i].In2Node := fNodes[j];
    End;
  End;
  // Die Verbindung zum Finalbild
  j := -1;
  f.Read(j, SizeOf(j));
  If j <> -1 Then Begin
    fResultingImage.InNode := fNodes[j];
  End;
  f.free;
  fchanged := false;
  SetCaption;
  PaintBox1.Invalidate;
  OpenGLControl1Paint(Nil);
End;

Procedure TForm1.Clear;
Var
  i: Integer;
Begin
  fConnector.Connector := ctnone;
  For i := 0 To high(fNodes) Do Begin
    fNodes[i].Free;
  End;
  setlength(fNodes, 0);
  fResultingImage.InNode := Nil;
  fchanged := false;
  fFilename := '';
  SetCaption;
End;

Procedure TForm1.SetEvents(Var Element: TNode);
Begin
  Element.OnCloseButtonClick := @OnNodeCloseButtonClick;
  Element.OnOutConnectClick := @OnOutConnectorClick;
  Element.OnChange := @OpenGLControl1Paint;
  Element.OnHint := @OnHint;
  If Element Is TTwoInputNode Then Begin
    (Element As TTwoInputNode).OnInButtonClick := @OnInConnectorClick;
    (Element As TTwoInputNode).OnInButton2Click := @OnInConnector2Click;
  End
  Else Begin
    (Element As TOneInputNode).OnInButtonClick := @OnInConnectorClick;
  End;
End;

Procedure TForm1.CreateListBoxMenu;
Var
  sl: TStringList;
  i: Integer;
Begin
  sl := Form2.GetOPList;
  SetLength(fOperators, 1);
  ListBox1.Clear;
  ListBox1.Items.Add('cSource');
  // Der Operator Bild Qelle
  ListBox1.Items.Add('eImage');
  fOperators[0].Name := 'Image';
  fOperators[0].Element := TImageSource;
  ListBox1.Items.Add('cPixel operations');
  InitializeOperations(@AddOperatorToList);
  // Einfügen der User Generierten Operatoren
  For i := 0 To sl.Count - 1 Do Begin
    If lowercase(ExtractFileExt(sl[i])) = '.op' Then Begin
      SetLength(fOperators, high(fOperators) + 2);
      fOperators[high(fOperators)].Name := ExtractFileNameOnly(sl[i]) + ';' + sl[i];
      fOperators[high(fOperators)].Element := TCustomOperator;
      ListBox1.Items.Add('o' + ExtractFileNameOnly(sl[i]));
    End;
  End;
  ListBox1.Items.Add('cBlend operations');
  InitializeBlends(@AddOperatorToList);
  // Einfügen der User Generierten Blenden
  For i := 0 To sl.Count - 1 Do Begin
    If lowercase(ExtractFileExt(sl[i])) = '.blend' Then Begin
      SetLength(fOperators, high(fOperators) + 2);
      fOperators[high(fOperators)].Name := ExtractFileNameOnly(sl[i]) + ';' + sl[i];
      fOperators[high(fOperators)].Element := TCustomBlender;
      ListBox1.Items.Add('b' + ExtractFileNameOnly(sl[i]));
    End;
  End;
  sl.free;
  PaintBox1.Invalidate;
  OpenGLControl1Paint(Nil);
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  f: String;
Begin
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'settings.ini');
  fZoom := 1;
  fdefcaption := 'Imageshop by Corpsman www.Corpsman.de ver. ' + format('%1.2f', [Version / 100]);
  fFilename := '';
  fchanged := false;
  SetCaption;
  f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations';
  If Not DirectoryExists(f) Then Begin
    If Not CreateDir(f) Then Begin
      showmessage('Error could not create : ' + f);
      halt;
    End;
  End;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  fConnector.Connector := ctnone;
  Panel1.Caption := '';
  ListBox1.Align := alLeft;
  Panel1.Align := alClient;
  PaintBox1.Align := alBottom;
  Splitter1.Align := alBottom;
  OpenGLControl1.Align := alClient;
  fResultingImage := TImageDest.Create(PaintBox1, 50, 50);
  fResultingImage.OnInButtonClick := @OnInConnectorClick;
  fResultingImage.OnChange := @OpenGLControl1Paint;
  fNodes := Nil;
  fSelectedElement := Nil;
  CreateListBoxMenu;
{$IFDEF Windows}
  ListBox1.ItemHeight := ListBox1.Canvas.TextHeight('Hq') + 4;
{$ENDIF}
  f := ini.ReadString('General', 'LastScene', '');
  If (f <> '') And FileExists(f) Then Begin
    LoadScene(f);
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If fchanged Then Begin
    If ID_NO = Application.MessageBox('You did not save yet, are you shure you want to close?', 'Warning', MB_YESNO Or MB_ICONQUESTION) Then Begin
      CanClose := false;
    End;
  End;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  ini.WriteString('General', 'LastScene', fFilename);
  Clear;
  fResultingImage.free;
  ini.free;
End;

Procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Begin
  If length(ListBox1.items[index]) = 0 Then exit;
  ListBox1.Canvas.Pen.Color := clwhite;
  ListBox1.canvas.Font.Color := clblack;
  If ListBox1.items[index][1] = 'c' Then Begin
    // Eine Überschrift
    ListBox1.Canvas.Brush.Color := clGray;
  End
  Else Begin
    ListBox1.Canvas.Brush.Color := clWhite;
    // Ein Eintrag
    If (odSelected In state) Then Begin
      ListBox1.Canvas.Pen.Color := clred;
    End;
  End;
  ListBox1.Canvas.Rectangle(ARect);
  ListBox1.Canvas.TextOut(ARect.Left + 3, ARect.Top + (ARect.Height - canvas.TextHeight(ListBox1.items[index])) Div 2, copy(ListBox1.items[index], 2, length(ListBox1.items[index])));
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Clear All
  clear;
  PaintBox1.Invalidate;
  OpenGLControl1Paint(Nil);
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Save
  If fFilename <> '' Then Begin
    SaveScene(fFilename);
  End
  Else Begin
    MenuItem5Click(Nil);
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Save as
  If SaveDialog1.Execute Then Begin
    SaveScene(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    LoadScene(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  form2.RefreshOPList();
  form2.showmodal;
  CreateListBoxMenu;
End;

Var
  allowcnt: Integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Var
  bma, bm: Tbitmap;
Begin
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    OpenGL_GraphikEngine.clear;
    glenable(GL_TEXTURE_2D); // Texturen
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less);
    bm := TBitmap.Create;
    bm.Width := 32;
    bm.Height := 32;
    bm.PixelFormat := pf24bit;
    bma := TBitmap.Create;
    bma.Width := 32;
    bma.Height := 32;
    bma.PixelFormat := pf24bit;
    bma.Canvas.brush.Color := clwhite;
    bma.Canvas.brush.Style := bsSolid;
    bma.Canvas.Rectangle(-1, -1, 33, 33);
    ImageList1.Draw(bm.Canvas, 0, 0, 0);
    backtex := OpenGL_GraphikEngine.LoadAlphaGraphik(bm, bma, 'backtex');
    bma.free;
    bm.free;
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  fZoom := fZoom * 1.1;
  OpenGLControl1Paint(Nil);
End;

Procedure TForm1.OpenGLControl1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  fZoom := fZoom / 1.1;
  OpenGLControl1Paint(Nil);
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  dim: TPoint;
  i, j: Integer;
  v: TPixel;
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  dim := fResultingImage.GetImageDimension;
  Go2d();
  // Hintergrund Malen
  glPushMatrix();
  glTranslatef(0, 0, -0.5);
  glColor4f(1, 1, 1, 1);
  For i := 0 To (OpenGLControl1.Width Div 32) + 1 Do Begin
    For j := 0 To (OpenGLControl1.Height Div 32) + 1 Do Begin
      RenderQuad(point(i * 32, j * 32), point((i + 1) * 32, (j + 1) * 32), 0, false, backtex);
    End;
  End;
  glPopMatrix();
  // Wenn es Ein Ausgabebild gibt wird das gemalt
  If dim.x <> 0 Then Begin
    glenable(gl_Blend);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
    glPushMatrix();
    glTranslatef((OpenGLControl1.Width - dim.x * fZoom) / 2, (OpenGLControl1.Height - dim.y * fZoom) / 2, 0);
    glScalef(fZoom, fZoom, 1);
    glPointSize(fZoom * 1.45); // Die Pixelsize ein Ticken Größer, damit es weniger störungen gibt
    glbegin(GL_POINTS);
    For i := 0 To dim.x - 1 Do Begin
      For j := 0 To dim.y - 1 Do Begin
        v := fResultingImage.GetPixelValue(i / (dim.x - 1), j / (dim.y - 1));
        glColor4fv(@v);
        glVertex2f(i, j);
      End;
    End;
    glend;
    gldisable(gl_blend);
    glPopMatrix();
  End;
  Exit2d();
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
  End;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  i: integer;
  elem: String;
Begin
  fSelectedElement := Nil;
  If (ListBox1.ItemIndex > 0) Then Begin
    // Als Quellcode vorliegende Klassen
    If (ListBox1.Items[ListBox1.ItemIndex][1] = 'e') Then Begin
      elem := copy(ListBox1.Items[ListBox1.ItemIndex], 2, length(ListBox1.Items[ListBox1.ItemIndex]));
      // Einfügen des Elementes
      For i := 0 To high(fOperators) Do Begin
        If fOperators[i].Name = elem Then Begin
          SetChanged;
          setlength(fnodes, high(fnodes) + 2);
          fnodes[high(fnodes)] := fOperators[i].Element.create(paintbox1, x - x Mod GridSize, y - y Mod GridSize);
          SetEvents(fnodes[high(fnodes)]);
        End;
      End;
      // Den Listbox Index wegschieben
      ListBox1.ItemIndex := 0; // der erste Eintrag ist immer "cSource"
      PaintBox1.invalidate;
    End;
    // Als Datei Abliegende Operator Klasse
    If (ListBox1.Items[ListBox1.ItemIndex][1] = 'o') Then Begin
      elem := copy(ListBox1.Items[ListBox1.ItemIndex], 2, length(ListBox1.Items[ListBox1.ItemIndex]));
      For i := 0 To high(fOperators) Do Begin
        If pos(elem + ';', fOperators[i].Name) = 1 Then Begin
          elem := copy(fOperators[i].Name, pos(';', fOperators[i].Name) + 1, length(fOperators[i].Name));
          SetChanged;
          setlength(fNodes, high(fNodes) + 2);
          fNodes[high(fNodes)] := TCustomOperator.Create(paintbox1, x - x Mod GridSize, y - y Mod GridSize);
          SetEvents(fnodes[high(fnodes)]);
          If Not TCustomOperator(fNodes[high(fNodes)]).LoadSourceCode(elem) Then Begin
            showmessage('Error could not load : ' + elem + LineEnding +
              'Is the source correct ?');
            fNodes[high(fNodes)].free;
            setlength(fnodes, high(fnodes));
          End;
        End;
      End;
      ListBox1.ItemIndex := 0; // der erste Eintrag ist immer "cSource"
      PaintBox1.invalidate;
    End;
    // Als Datei Abliegende Blend Operator Klasse
    If (ListBox1.Items[ListBox1.ItemIndex][1] = 'b') Then Begin
      elem := copy(ListBox1.Items[ListBox1.ItemIndex], 2, length(ListBox1.Items[ListBox1.ItemIndex]));
      For i := 0 To high(fOperators) Do Begin
        If pos(elem + ';', fOperators[i].Name) = 1 Then Begin
          elem := copy(fOperators[i].Name, pos(';', fOperators[i].Name) + 1, length(fOperators[i].Name));
          SetChanged;
          setlength(fNodes, high(fNodes) + 2);
          fNodes[high(fNodes)] := TCustomBlender.Create(paintbox1, x - x Mod GridSize, y - y Mod GridSize);
          SetEvents(fnodes[high(fnodes)]);
          If Not TCustomBlender(fNodes[high(fNodes)]).LoadSourceCode(elem) Then Begin
            showmessage('Error could not load : ' + elem + LineEnding +
              'Is the source correct ?');
            fNodes[high(fNodes)].free;
            setlength(fnodes, high(fnodes));
          End;
        End;
      End;
      ListBox1.ItemIndex := 0; // der erste Eintrag ist immer "cSource"
      PaintBox1.invalidate;
    End;
  End
  Else Begin
    // Der User Clickt evtl ein Element an ?
    fSelectedElement := GetSelectedElement(x, y);
    If assigned(fSelectedElement) Then Begin
      fMouseDownDelta := point(x - fSelectedElement.Left, y - fSelectedElement.Top);
    End;
  End;
End;

Procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  If assigned(fSelectedElement) Then Begin
    SetChanged;
    fSelectedElement.Left := x - fMouseDownDelta.x;
    fSelectedElement.Left := fSelectedElement.Left - fSelectedElement.Left Mod GridSize;
    fSelectedElement.Top := y - fMouseDownDelta.y;
    fSelectedElement.Top := fSelectedElement.Top - fSelectedElement.Top Mod GridSize;
    PaintBox1.Invalidate;
  End;
  If fConnector.Connector <> ctnone Then Begin
    PaintBox1Paint(Nil);
    PaintBox1.Canvas.Line(fConnector.Position, point(x, y));
  End;
End;

Procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  fSelectedElement := Nil;
  // Alles Wieder Löschen
  fConnector.Connector := ctnone;
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  i: integer;
Begin
  // Clear
  PaintBox1.Canvas.Brush.Color := clsilver;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Pen.Color := clSilver;
  PaintBox1.Canvas.Rectangle(PaintBox1.ClientRect);
  // Render Scene
  fResultingImage.Render;
  For i := 0 To high(fnodes) Do Begin
    fnodes[i].render;
  End;
End;

Procedure TForm1.PaintBox1Resize(Sender: TObject);
Begin
  fResultingImage.Left := PaintBox1.Width - fResultingImage.Width - 10;
  fResultingImage.Left := fResultingImage.Left - fResultingImage.Left Mod GridSize;
  fResultingImage.Top := (PaintBox1.Height - fResultingImage.Height) Div 2;
  fResultingImage.Top := fResultingImage.Top - fResultingImage.Top Mod GridSize;
End;

End.

