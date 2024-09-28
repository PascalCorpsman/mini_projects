(******************************************************************************)
(* Wave function collapse (tile model)                             17.01.2024 *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of wave function collapse algorithm           *)
(*               (Tile-Mode)                                                  *)
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
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Backjumping (like backtracking but with jumps)        *)
(*               0.03 - Cleanup                                               *)
(*               0.04 - Add feature stop on miss                              *)
(*               0.05 - Export as PNG                                         *)
(*               0.06 - Export of big images                                  *)
(*                                                                            *)
(******************************************************************************)
// Inspired by https://www.youtube.com/watch?v=rI_y2GAlQFM
(*
 Ideen: Man könnte eine "Beschränkung" einbauen, dass ein Teil Maximal X-Mal generiert werden darf
        Auf diese weise könnte man z.B. Weggabelungen reglementieren

 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, uwfc;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog2: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    PaintBox1: TPaintBox;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBox1: TScrollBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit2KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit3KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit4KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit5KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Image1Click(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
  private
    wfc: Twfc;
    SelectedIndex: integer;
    Images: TWVCImageArray;
    Procedure AppendImageToLCL(index: integer);
    Procedure Clear;
    Procedure AddImage(Const Filename: String);
    Procedure SetSelectedImage(Index: Integer);

    Procedure SaveSystem(Const Filename: String);
    Function LoadSystem(Const Filename: String): Boolean;

    Procedure OnCollapseCell(Sender: TObject);
    Procedure OnRenderTooLong(Sender: TObject);
    Procedure RenderWFCToCanvas(Const aCanvas: TCanvas);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses IniFiles;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Wave Function Collapse Demo ver. 0.06';
  // Aufräumen, der Entwickler Hilfen
  edit1.free;
  edit2.free;
  edit3.free;
  edit4.free;
  edit5.free;
  Image1.Free;
  edit6.text := '10';
  edit7.text := '10';
  edit8.text := '10';
  Randomize;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  wfc := Twfc.create;
  wfc.OnUpdate := @OnCollapseCell;
  wfc.OnRenderTooLong := @OnRenderTooLong;
  // Load a default tileset if one exists ;)
  If Loadsystem('paths.sys') Then Begin
    Button6.Click;
  End;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  wfc.free;
  wfc := Nil;
  Clear;
End;

Procedure TForm1.Image1Click(Sender: TObject);
Var
  index: Integer;
Begin
  index := TImage(sender).Tag;
  SetSelectedImage(index);
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  w, h: Integer;
Begin
  If SelectedIndex = -1 Then exit;
  If SelectedIndex > high(Images) Then exit;
  Button7.Click;
  w := Images[SelectedIndex].Bitmap.Width;
  h := Images[SelectedIndex].Bitmap.Height;
  x := x Div w;
  y := y Div h;
  If ssleft In shift Then Begin
    wfc.Grid[x, y].Forced := true;
    wfc.Grid[x, y].Index := SelectedIndex;
    CheckBox2.Checked := true;
  End
  Else Begin
    wfc.Grid[x, y].Forced := false;
    wfc.Grid[x, y].Index := -1;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Begin
  // Clear Back
  paintbox1.Canvas.Brush.Color := clRed;
  paintbox1.Canvas.Rectangle(-1, -1, PaintBox1.Width + 1, PaintBox1.Height + 1);
  RenderWFCToCanvas(PaintBox1.Canvas);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i: Integer;
  fn, ap, frn: String;
  sl: TStringList;
Begin
  // Import Images
  If OpenPictureDialog1.Execute Then Begin
    Clear;
    sl := TStringList.Create;
    sl.Sorted := true;
    For i := 0 To OpenPictureDialog1.Files.Count - 1 Do Begin
      sl.Add(OpenPictureDialog1.Files[i]);
    End;
    sl.Sort;
    For i := 0 To OpenPictureDialog1.Files.Count - 1 Do Begin
      fn := sl[i];
      ap := ExtractFilePath(ParamStr(0));
      frn := ExtractRelativePath(ap, fn);
      AddImage(frn);
    End;
    sl.free;
    If assigned(Images) Then Begin
      //      SetSelectedImage(0);
    End
    Else Begin
      SetSelectedImage(-1);
    End;
    button3.click;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Load System
  If OpenDialog2.Execute Then Begin
    Loadsystem(OpenDialog2.FileName);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  w, h, i, j: integer;
Begin
  // Set Dimension
  If Not assigned(Images) Then exit;
  w := StrToIntdef(Edit6.Text, 0);
  h := StrToIntdef(Edit7.Text, 0);
  PaintBox1.Width := w * Images[0].Bitmap.Width;
  PaintBox1.Height := h * Images[0].Bitmap.Height;
  setlength(wfc.Grid, w, h);
  For i := 0 To w - 1 Do Begin
    For j := 0 To h - 1 Do Begin
      wfc.Grid[i, j].Index := -1;
      wfc.Grid[i, j].Forced := false;
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  // Create
  If button6.caption = 'Cancel' Then Begin
    wfc.Cancel := true;
    button6.caption := 'Create';
    Button6.Enabled := false;
    exit;
  End;
  button6.caption := 'Cancel';
  Application.ProcessMessages;

  // 2. Grid Initialisieren
  If Not assigned(wfc.Grid) Then button3.Click;
  If Not assigned(wfc.Grid) Then Begin
    button6.caption := 'Create';
    Button6.Enabled := true;
    exit;
  End;

  wfc.LoadImages(Images);
  wfc.StopOnMis := CheckBox3.Checked;
  wfc.Run();

  PaintBox1.Invalidate;
  button6.caption := 'Create';
  Button6.Enabled := true;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Reset Grid
  wfc.ResetGrid;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  Showmessage(
    '1. Load images and set "connectors" or load a system' + LineEnding +
    '2. Set dimension and create' + LineEnding +
    '[Optional]' + LineEnding +
    '3. Click on a image on the left side to "select"' + LineEnding +
    '4. Click on the preview area to set element (right click removes)' + LineEnding +
    LineEnding +
    'If you get red areas after create, this means that your model is not "connected" enough.'
    );
End;

Procedure TForm1.Button9Click(Sender: TObject);
Var
  bm: TBitmap;
  png: TPortableNetworkGraphic;
  f: Boolean;
Begin
  // Export Image
  If SaveDialog2.Execute Then Begin
    bm := TBitmap.Create;
    bm.Width := PaintBox1.Width;
    bm.Height := PaintBox1.Height;
    f := CheckBox2.Checked;
    If f Then Begin
      CheckBox2.Checked := false;
      PaintBox1.Invalidate;
      Application.ProcessMessages;
    End;
    //    bm.Canvas.CopyRect(rect(0, 0, bm.Width, bm.Height), PaintBox1.Canvas, rect(0, 0, bm.Width, bm.Height));
    RenderWFCToCanvas(bm.Canvas);
    If f Then Begin
      CheckBox2.Checked := true;
      PaintBox1.Invalidate;
      Application.ProcessMessages;
    End;
    If lowercase(ExtractFileExt(SaveDialog2.FileName)) = '.png' Then Begin
      png := TPortableNetworkGraphic.Create;
      png.Assign(bm);
      png.SaveToFile(SaveDialog2.FileName);
      png.free;
    End
    Else Begin
      bm.SaveToFile(SaveDialog2.FileName);
    End;
    bm.free;
  End;
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox3Click(Sender: TObject);
Begin
  wfc.StopOnMis := CheckBox3.Checked;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConLeft] := Tedit(sender).Text;
End;

Procedure TForm1.Edit2KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConUp] := Tedit(sender).Text;
End;

Procedure TForm1.Edit3KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConRight] := Tedit(sender).Text;
End;

Procedure TForm1.Edit4KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConDown] := Tedit(sender).Text;
End;

Procedure TForm1.Edit5KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Prop := strtointdef(Tedit(sender).Text, 0);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  wfc.cancel := true;
End;

Procedure TForm1.Clear;
Var
  i: Integer;
Begin
  For i := ScrollBox1.ComponentCount - 1 Downto 0 Do Begin
    ScrollBox1.Components[i].free;
  End;
  For i := 0 To high(Images) Do Begin
    Images[i].Bitmap.Free;
  End;
  setlength(Images, 0);
End;

Procedure TForm1.AppendImageToLCL(index: integer);
Var
  img: TImage;
  e: TEdit;
  t: Integer;
Begin
  // Übernehmen in die Gui
  t := 8 + index * 128;
  // Das Wahrscheinlichkeits Edit
  e := TEdit.Create(ScrollBox1);
  e.Name := 'Prop' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t;
  e.Left := 8;
  e.Width := 50;
  e.Tag := index;
  e.Text := inttostr(Images[index].Prop);
  e.OnKeyUp := @Edit5KeyUp;
  e.Hint := 'Propabillity for the image' + LineEnding +
    '0 = disabled' + LineEnding +
    'The higher the value, the more higher the propabillity for it to come up.';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Left' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t + 48;
  e.Left := 8;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConLeft];
  e.OnKeyUp := @Edit1KeyUp;
  e.Hint := 'Definition of the "left" connector, this has to match the "right" connector of the other images';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Top' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t;
  e.Left := 64;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConUp];
  e.OnKeyUp := @Edit2KeyUp;
  e.Hint := 'Definition of the "top" connector, this has to match the "bottom" connector of the other images';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Right' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t + 48;
  e.Left := 120;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConRight];
  e.OnKeyUp := @Edit3KeyUp;
  e.Hint := 'Definition of the "right" connector, this has to match the "left" connector of the other images';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Down' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t + 88;
  e.Left := 64;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConDown];
  e.OnKeyUp := @Edit4KeyUp;
  e.Hint := 'Definition of the "bottom" connector, this has to match the "top" connector of the other images';
  e.ShowHint := true;

  img := TImage.Create(ScrollBox1);
  img.Name := 'Img' + inttostr(index + 1);
  img.Parent := ScrollBox1;
  img.Top := t + 32;
  img.Left := 64;
  img.AutoSize := false;
  img.Width := 50;
  img.Height := 50;
  img.Center := true;
  img.Stretch := true;
  img.Picture.Assign(Images[index].Bitmap);
  img.Tag := index;
  img.OnClick := @Image1Click;
End;

Procedure TForm1.AddImage(Const Filename: String);
Var
  i: Integer;
Begin
  // Eintragen in die Images
  setlength(Images, high(Images) + 2);
  Images[high(Images)].Filename := Filename;
  Images[high(Images)].Bitmap := TBitmap.Create;
  Images[high(Images)].Bitmap.LoadFromFile(Filename);
  Images[high(Images)].Prop := 100;
  For i := 0 To 3 Do Begin
    Images[high(Images)].Connectors[i] := '';
  End;
  // PreCheck und ggf raus
  If high(Images) <> 0 Then Begin
    If (Images[0].Bitmap.Width <> Images[High(Images)].Bitmap.Width) Or
    (Images[0].Bitmap.Height <> Images[High(Images)].Bitmap.Height) Then Begin
      showmessage('Error, all images need to have same dimension, skipping: ' + Filename);
      Images[high(Images)].Bitmap.free;
      setlength(Images, high(Images));
      exit;
    End;
  End;
  AppendImageToLCL(high(Images));
End;

Procedure TForm1.SetSelectedImage(Index: Integer);
Begin
  If index >= 0 Then Begin
    Image2.Picture.Assign(Images[index].Bitmap);
  End
  Else Begin
    Image2.Picture.Clear;
  End;
  SelectedIndex := index;
End;

Procedure TForm1.SaveSystem(Const Filename: String);
Var
  ini: tinifile;
  i: Integer;
Begin
  ini := TIniFile.Create(FileName);
  ini.CacheUpdates := true;
  ini.WriteInteger('Images', 'Count', length(images));
  For i := 0 To high(images) Do Begin
    ini.WriteString('Images', 'Image' + IntToStr(i), images[i].Filename);
    ini.WriteInteger('Images', 'ImageP' + IntToStr(i), images[i].Prop);
    ini.WriteString('Images', 'ImageL' + IntToStr(i), images[i].Connectors[ConLeft]);
    ini.WriteString('Images', 'ImageR' + IntToStr(i), images[i].Connectors[ConRight]);
    ini.WriteString('Images', 'ImageU' + IntToStr(i), images[i].Connectors[ConUp]);
    ini.WriteString('Images', 'ImageD' + IntToStr(i), images[i].Connectors[ConDown]);
  End;
  ini.Free;
End;

Function TForm1.LoadSystem(Const Filename: String): Boolean;
Var
  ini: TIniFile;
  i: Integer;
Begin
  result := false;
  Clear;
  If Not FileExists(Filename) Then exit;
  result := true;
  ini := TIniFile.Create(Filename);
  setlength(Images, ini.ReadInteger('Images', 'Count', 0));
  For i := 0 To high(Images) Do Begin
    images[i].Filename := ini.ReadString('Images', 'Image' + IntToStr(i), '');
    images[i].Bitmap := TBitmap.Create;
    images[i].Bitmap.LoadFromFile(images[i].Filename);
    images[i].Prop := ini.ReadInteger('Images', 'ImageP' + IntToStr(i), 100);
    images[i].Connectors[ConLeft] := ini.ReadString('Images', 'ImageL' + IntToStr(i), '');
    images[i].Connectors[ConRight] := ini.ReadString('Images', 'ImageR' + IntToStr(i), '');
    images[i].Connectors[ConUp] := ini.ReadString('Images', 'ImageU' + IntToStr(i), '');
    images[i].Connectors[ConDown] := ini.ReadString('Images', 'ImageD' + IntToStr(i), '');
    AppendImageToLCL(i);
  End;
  ini.free;
  SetSelectedImage(-1);
  button3.Click;
End;

Procedure TForm1.OnCollapseCell(Sender: TObject);
Begin
  If CheckBox1.Checked Then Begin
    PaintBox1.Invalidate;
    Application.ProcessMessages;
    sleep(strtointdef(edit8.text, 10));
  End;
End;

Procedure TForm1.OnRenderTooLong(Sender: TObject);
Begin
  If CheckBox1.Checked Then exit;
  Showmessage(
    'Activateing preview now, as it seem to be to difficult to create the map.' + LineEnding +
    'Click on "Cancel" button to skip actual creation'
    );
  button6.caption := 'Cancel';
  CheckBox1.Checked := true;
  edit8.text := '1';
  Button6.Enabled := true;
End;

Procedure TForm1.RenderWFCToCanvas(Const aCanvas: TCanvas);
Var
  i, j: Integer;
Begin
  If Not assigned(wfc.Grid) Then exit;
  For i := 0 To high(wfc.Grid) Do Begin
    For j := 0 To high(wfc.Grid[i]) Do Begin
      If wfc.Grid[i, j].Index <> -1 Then Begin
        aCanvas.Draw(i * Images[0].Bitmap.Width, j * Images[0].Bitmap.Height, Images[wfc.Grid[i, j].Index].Bitmap);
      End;
      If (wfc.Grid[i, j].Forced And CheckBox2.Checked) Or
        (CheckBox3.Checked And (wfc.InvalidPos.X = i) And (wfc.InvalidPos.Y = j))
        Then Begin
        aCanvas.Pen.Color := clred;
        If (CheckBox3.Checked And (wfc.InvalidPos.X = i) And (wfc.InvalidPos.Y = j)) Then Begin
          aCanvas.Pen.Color := clblue;
        End;
        aCanvas.MoveTo((i + 0) * Images[0].Bitmap.Width, (j + 0) * Images[0].Bitmap.Height);

        aCanvas.LineTo((i + 1) * Images[0].Bitmap.Width - 1, (j + 0) * Images[0].Bitmap.Height);
        aCanvas.LineTo((i + 1) * Images[0].Bitmap.Width - 1, (j + 1) * Images[0].Bitmap.Height - 1);
        aCanvas.LineTo((i + 0) * Images[0].Bitmap.Width, (j + 1) * Images[0].Bitmap.Height - 1);
        aCanvas.LineTo((i + 0) * Images[0].Bitmap.Width, (j + 0) * Images[0].Bitmap.Height);
      End;
    End;
  End;

End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    SaveSystem(SaveDialog1.FileName);
  End;
End;

End.

