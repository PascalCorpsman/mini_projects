(******************************************************************************)
(* Button_Tool                                                     ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tool to create images of "buttons" with fancy fonts settings *)
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
(* History     : 0.01 - 0.03 : Initial version / Porting to Lazarus / Linux   *)
(*               0.04 - OpenSource / Insert the "Underlay texture"            *)
(*               0.05 - Bugfix in load settings routine.                      *)
(*               0.06 - Kleinere Schönheitsfehler korrigiert (Nil-Pointer,    *)
(*                      zusätzliche Fehlermeldungen..)                        *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Math, IntfGraphics, fpImage, LCLType, lazpng, lazjpg;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
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
    CheckBox4: TCheckBox;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenDialog3: TOpenDialog;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBar1: TScrollBar;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure CheckBox4Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit3Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure RenderCaption(Color_: TColor; Const BM: TBitmap);
    Procedure UpdateFont(Fontname: String; FontSize: Integer; FontStyle: TFontStyles);
  End;

Var
  Form1: TForm1;
  ResultButton: Tbitmap = Nil; // Ergebnissbild
  ButtonOverlay: TBitmap = Nil; // Die Textur die als FontTextur benutzt wird
  ButtonOverlayFile: String;
  ButtonMask: Tbitmap = Nil; // Maske im Hintergrund
  ButtonMaskFile: String; // Dateiname der Maske ( Fürs Speichern der ButtonSettings )
  ButtonFont: TFont; // Font Eigentschaftem
  fontfile: String = ''; // Externe TTF Font Dateiname
  Schwellwert: integer = 128;

Implementation

{$R *.lfm}

{ TForm1 }

Uses LazFileUtils;

(*
Installiert eine TTF im System
*)

Function LoadFontByString(FontFile_: String): boolean;
Var
  home: String;
Begin
  Result := False;
{$IFDEF WINDOWS}
  home := IncludeTrailingPathDelimiter(getenvironmentvariable('WINDIR')) + 'Fonts' + PathDelim;
  fontfile := FontFile_;
  If FileExistsUTF8(home + ExtractFileName(FontFile_)) Then Begin
    Result := True;
  End
  Else Begin
    Result := CopyFile(FontFile_, home + ExtractFileName(FontFile_));
  End;
{$ENDIF}
{$IFDEF LINUX}
  // Home Verzeichniss auslesen
  home := IncludeTrailingBackslash(getenvironmentvariable('HOME'));
  Result := False;
  fontfile := FontFile_;
  If Not DirectoryExists(home + '.fonts') Then Begin
    If Not CreateDir(home + '.fonts') Then
      exit;
  End;
  If Not FileExists(home + '.fonts/' + ExtractFileName(FontFile_)) Then Begin
    // Installieren der Font
    If CopyFile(FontFile_, home + '.fonts/' + ExtractFileName(FontFile_)) Then Begin
      ShowMessage('Font installed sucessfully, please restart the application to take the effect');
      Result := True;
    End;
  End
  Else Begin
    // Schrift ist schon installiert
    Result := True;
  End;
{$ENDIF}
End;

Function Luminance(C: TFPColor): byte;
Begin
  //   Y = 0.3R + 0.59G + 0.11B
  Result := min(255, max(0, round(0.3 * (c.red Shr 8) + 0.59 *
    (c.green Shr 8) + 0.11 * (c.blue Shr 8))));
End;

Function CompareFPColor(c1, c2: TFPColor): boolean;
Begin
  Result := (c1.red = c2.red) And (c1.green = c2.green) And (c1.blue = c2.blue);
End;

Function ColorToFPColor(Const Color: TColor): TFPColor;
Begin
  Result.alpha := 0;
  Result.red := (byte(Color) Shl 8) Or $FF;
  Result.green := ((Color) And $FF00) Or $FF;
  Result.blue := (((Color) And $FF0000) Shr 8) Or $FF;
End;

Procedure TForm1.RenderCaption(Color_: TColor; Const BM: TBitmap);
Var
  cap: String;
  capw, caph: integer;
  l, r, i, dis: integer;
  b: boolean;
  c: char;
Begin
  // Die Caption darauf Rendern
  BM.Canvas.Font.Color := Color_;
  BM.Canvas.Font.Style := ButtonFont.Style;
  BM.Canvas.Font.Name := ButtonFont.Name;
  BM.Canvas.Font.Size := ButtonFont.Size;
  cap := edit1.Text;
  capw := BM.Canvas.TextWidth(cap);
  caph := BM.Canvas.Textheight(cap);
  BM.Canvas.TextOut((BM.Width - capw) Div 2,
    (BM.Height - caph) Div 2, cap);
  // Der "Underline" Strich
  If length(edit5.Text) <> 0 Then Begin
    dis := strtointdef(edit6.Text, 0);
    c := lowercase(edit5.Text)[1];
    b := False;
    i := 1;
    While (i < length(cap)) And Not b Do Begin
      // Wenn wir die Position haben an derer der "Unterstrich" gesetzt werden soll
      If Lowercase(cap)[i] = c Then Begin
        l := BM.Canvas.TextWidth(copy(cap, 1, i - 1));
        r := BM.Canvas.TextWidth(copy(cap, 1, i));
        BM.Canvas.Pen.Width := max(1, strtointdef(edit7.Text, 1));
        BM.Canvas.Pen.Color := Color_;
        BM.Canvas.MoveTo((BM.Width - capw) Div 2 + l, (BM.Height - caph) Div
          2 + caph + dis);
        BM.Canvas.LineTo((BM.Width - capw) Div 2 + r, (BM.Height - caph) Div
          2 + caph + dis);
        b := True;
      End;
      Inc(i);
    End;
  End;
End;

Procedure TForm1.UpdateFont(Fontname: String; FontSize: Integer; FontStyle: TFontStyles);
Begin
  ButtonFont.Name := FontName;
  ButtonFont.Size := FontSize;
  ButtonFont.Style := FontStyle;
  // Übrenehmen in die Edits / Checkboxen
  Edit2.Text := ButtonFont.Name;
  Edit2.Font.Color := ButtonFont.Color;
  Edit3.Text := IntToStr(ButtonFont.Size);
  CheckBox1.Checked := fsBold In ButtonFont.Style;
  CheckBox2.Checked := fsItalic In ButtonFont.Style;
  fontfile := '';
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
End;

Var
  Mask: Array Of Array Of shortint;

Procedure CreateMaskData(Color_: TColor; Const BM: TBitmap);
Var
  i, j: integer;
  transc: TFPColor;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  // Init
  setlength(Mask, bm.Width, bm.Height);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(bm.Handle, bm.MaskHandle);
  transc := ColorToFPColor(clwhite);
  For i := 0 To bm.Width - 1 Do
    For j := 0 To bm.Height - 1 Do Begin
      If CompareFPColor(transc, TempIntfImg.Colors[i, j]) Then Begin
        mask[i, j] := 1;
      End
      Else If Luminance(TempIntfImg.Colors[i, j]) >= Schwellwert Then
        mask[i, j] := 1
      Else
        mask[i, j] := 0;
    End;
  transc := ColorToFPColor(color_);
  For i := 0 To bm.Width - 1 Do
    For j := 0 To bm.Height - 1 Do Begin
      If mask[i, j] <= 0 Then
        TempIntfImg.Colors[i, j] := transc;
    End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  bm.Handle := ImgHandle;
  bm.MaskHandle := ImgMaskHandle;
  TempIntfImg.Free;
End;

Procedure CeateOverlay(Const ButtonOverlay, bm: TBitmap);
Var
  i, j: integer;
  over, tbm: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  tbm := TLazIntfImage.Create(0, 0);
  tbm.LoadFromBitmap(bm.Handle, bm.MaskHandle);
  over := TLazIntfImage.Create(0, 0);
  over.LoadFromBitmap(ButtonOverlay.Handle, ButtonOverlay.MaskHandle);
  For i := 0 To bm.Width - 1 Do Begin
    For j := 0 To bm.Height - 1 Do Begin
      If Mask[i, j] = 0 Then Begin
        tbm.Colors[i, j] := over.Colors[i, j];
      End;
    End;
  End;
  tbm.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  bm.Handle := ImgHandle;
  bm.MaskHandle := ImgMaskHandle;
  tbm.Free;
  over.Free;
End;

Procedure CreateBorder(color_: TColor; Width_: integer; Mode: boolean;
  Const bm: TBitmap);
Var
  swidth_, i, j, k, l: integer;
  tbm: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  c: TFPColor;
Begin
  c := ColorToFPColor(color_);
  tbm := TLazIntfImage.Create(0, 0);
  tbm.LoadFromBitmap(bm.Handle, bm.MaskHandle);
  swidth_ := sqr(width_);
  For i := width_ To bm.Width - 1 - Width_ Do Begin
    For j := width_ To bm.Height - 1 - Width_ Do Begin
      If Mask[i, j] = 0 Then Begin
        // Eckig
        If mode Then Begin
          For k := -width_ To width_ Do
            For l := -width_ To width_ Do Begin
              If mask[i + k, j + l] = 1 Then Begin
                mask[i + k, j + l] := -1;
                tbm.Colors[i + k, j + l] := c;
              End;
            End;
        End
          // Runde Ecken
        Else Begin
          For k := -width_ To width_ Do
            For l := -width_ To width_ Do Begin
              If sqr(k) + sqr(l) <= swidth_ Then
                If mask[i + k, j + l] = 1 Then Begin
                  mask[i + k, j + l] := -1;
                  tbm.Colors[i + k, j + l] := c;
                End;
            End;
        End;
      End;
    End;
  End;
  tbm.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  bm.Handle := ImgHandle;
  bm.MaskHandle := ImgMaskHandle;
  tbm.Free;
End;

Procedure LoadOverlay(Filename: String);
Var
  png: TPngimage;
  jpg: TJpgImage;
Begin
  If assigned(ButtonOverlay) Then
    ButtonOverlay.Free;
  ButtonOverlay := TBitmap.Create;
  If LowerCase(ExtractFileExt(Filename)) = '.png' Then Begin
    png := TPNGImage.Create;
    png.LoadFromFile(FileName);
    ButtonOverlay.Assign(png);
    png.Free;
  End
  Else If LowerCase(ExtractFileExt(Filename)) = '.jpg' Then Begin
    jpg := TjpgImage.Create;
    jpg.LoadFromFile(FileName);
    ButtonOverlay.Assign(jpg);
    jpg.Free;
  End
  Else Begin
    ButtonOverlay.LoadFromFile(FileName);
  End;
  form1.Image3.Picture.Assign(ButtonOverlay);
  ButtonOverlayFile := Filename;
End;

Procedure LoadMask(Filename: String);
Var
  png: TPngimage;
  jpg: TJpgImage;
Begin
  If assigned(ButtonMask) Then
    ButtonMask.Free;
  ButtonMask := TBitmap.Create;
  If LowerCase(ExtractFileExt(Filename)) = '.png' Then Begin
    png := TPNGImage.Create;
    png.LoadFromFile(FileName);
    ButtonMask.Assign(png);
    png.Free;
  End
  Else If LowerCase(ExtractFileExt(Filename)) = '.jpg' Then Begin
    jpg := TjpgImage.Create;
    jpg.LoadFromFile(FileName);
    ButtonMask.Assign(jpg);
    jpg.Free;
  End
  Else Begin
    ButtonMask.LoadFromFile(FileName);
  End;
  form1.Image1.Picture.Assign(ButtonMask);
  ButtonMaskFile := Filename;
End;

// Ausgabe des TmpImage ins ResultButton

Procedure DrawTmpToRes(Const bm1, bm2: TBitmap);
Var
  i, j: integer;
  tbm1, tbm2: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  tbm1 := TLazIntfImage.Create(0, 0);
  tbm1.LoadFromBitmap(bm1.Handle, bm1.MaskHandle);
  tbm2 := TLazIntfImage.Create(0, 0);
  tbm2.LoadFromBitmap(bm2.Handle, bm2.MaskHandle);
  For i := 0 To bm1.Width - 1 Do
    For j := 0 To bm1.Height - 1 Do
      If mask[i, j] < 1 Then
        tbm2.Colors[i, j] := tbm1.Colors[i, j];
  tbm2.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  bm2.Handle := ImgHandle;
  bm2.MaskHandle := ImgMaskHandle;
  tbm2.Free;
  tbm1.Free;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  tmpImage: TBitmap;
Begin
  If Not assigned(ButtonMask) Then
    exit;
  If assigned(ResultButton) Then
    ResultButton.Free;
  // Die Maske Reinladen
  ResultButton := TBitmap.Create;
  ResultButton.Assign(ButtonMask);
  // Wenn die Font Border an ist, muss mehr gemacht werden
  If CheckBox4.Checked Then Begin
    // Die Font wird in einem "Extra" Bild erzeugt und dann reingeladen
    tmpImage := TBitmap.Create;
    tmpImage.Width := ResultButton.Width;
    tmpImage.Height := ResultButton.Height;
    // Den Hintergrund Löschen
    tmpImage.Canvas.Pen.color := clwhite;
    tmpImage.Canvas.Brush.color := clwhite;
    tmpImage.Canvas.brush.style := bssolid;
    tmpImage.Canvas.Rectangle(-1, -1, tmpImage.Width + 1, tmpImage.Height + 1);
    // Berechnen der "Schrift überdeckt Maske"
    // Rendern der Schrift in Schwarz
    RenderCaption(clblack, tmpImage);
    // Umwandeln alles Dunkler als 32 in Buttonfont.color
    CreateMaskData(ButtonFont.Color, tmpImage);
    // Mask = True = Transparent alles ander = Schrift
    // Erzeugen der Textboarder in abhängigkeit des Mode
    CreateBorder(label5.font.color, strtointdef(edit4.Text, 1),
      RadioButton1.Checked, tmpImage);
    // Kopieren der nicht Transparenten Bildteile ins Ergebnissbild
    Drawtmptores(tmpImage, ResultButton);
    tmpImage.Free;
  End
  Else Begin
    RenderCaption(ButtonFont.Color, ResultButton);
  End;
  // Wenn das Overlay an ist
  If CheckBox3.Checked Then Begin
    // Die Font wird in einem "Extra" Bild erzeugt und dann reingeladen
    tmpImage := TBitmap.Create;
    tmpImage.Width := ResultButton.Width;
    tmpImage.Height := ResultButton.Height;
    // Den Hintergrund Löschen
    tmpImage.Canvas.Pen.color := clwhite;
    tmpImage.Canvas.Brush.color := clwhite;
    tmpImage.Canvas.brush.style := bssolid;
    tmpImage.Canvas.Rectangle(-1, -1, tmpImage.Width + 1, tmpImage.Height + 1);
    // Berechnen der "Schrift überdeckt Maske"
    // Rendern der Schrift in Schwarz
    RenderCaption(clblack, tmpImage);
    // Umwandeln alles Dunkler als 32 in schwarz
    CreateMaskData(clblack, tmpImage);
    // Alles was nun Schwarz ist muss aus der Overlay Textur geladen werden
    If assigned(ButtonOverlay) Then
      CeateOverlay(ButtonOverlay, tmpImage);
    // Kopieren der nicht Transparenten Bildteile ins Ergebnissbild
    Drawtmptores(tmpImage, ResultButton);
    tmpImage.Free;

  End;
  Image2.Picture.Assign(ResultButton);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  FontDialog1.Font.Name := ButtonFont.Name;
  FontDialog1.Font.Size := ButtonFont.Size;
  FontDialog1.Font.Style := ButtonFont.Style;
  If FontDialog1.Execute Then Begin
    UpdateFont(FontDialog1.Font.Name, FontDialog1.Font.Size, FontDialog1.Font.Style);
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  ColorDialog1.Color := ButtonFont.Color;
  If ColorDialog1.Execute Then Begin
    ButtonFont.Color := ColorDialog1.Color;
    edit2.font.color := ColorDialog1.Color;
    // Aktualisieren der Ausgabe
    Button2.OnClick(Nil);
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  ColorDialog1.Color := label5.font.color;
  If ColorDialog1.Execute Then Begin
    label5.font.color := ColorDialog1.Color;
    // Aktualisieren der Ausgabe
    Button2.OnClick(Nil);
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  png: Tpngimage;
  jpg: Tjpgimage;
Begin
  If SaveDialog1.Execute Then Begin
    SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
    // Aktualisieren der Ausgabe
    Button2.OnClick(Nil);
    If lowercase(ExtractFileExt(SaveDialog1.FileName)) = '.png' Then Begin
      png := TPNGImage.Create;
      png.Assign(ResultButton);
      png.SaveToFile(SaveDialog1.FileName);
      png.Free;
    End
    Else If lowercase(ExtractFileExt(SaveDialog1.FileName)) = '.jpg' Then Begin
      jpg := TjpgImage.Create;
      jpg.Assign(ResultButton);
      jpg.SaveToFile(SaveDialog1.FileName);
      jpg.Free;
    End
    Else Begin
      ResultButton.SaveToFile(SaveDialog1.FileName);
    End;
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  f: Textfile;
  s: String;
  fs: TFontStyles;
Begin
  If OpenDialog2.Execute Then Begin
    opendialog2.InitialDir := extractfilepath(opendialog2.Filename);
    SaveDialog2.InitialDir := extractfilepath(opendialog2.Filename);
    assignfile(f, opendialog2.Filename);
    reset(f);
    // Maske
    readln(f, s);
    If Fileexists(s) Then Begin
      LoadMask(s);
    End
    Else Begin
      showmessage('Error unable to locate : ' + LineEnding + s);
    End;
    // Font
    readln(f, s);
    If Fileexists(s) Then Begin
      LoadFontByString(s);
      Edit2.Text := ExtractFileNameOnly(s);
      ButtonFont.Name := Edit2.Text;
    End
    Else Begin
      fontfile := '';
      Edit2.Text := s;
    End;
    readln(f, s);
    edit2.font.color := stringtoColor(s);
    ButtonFont.color := stringtoColor(s);
    readln(f, s);
    edit3.Text := s;
    readln(f, s);
    checkbox1.Checked := s = '1';
    readln(f, s);
    checkbox2.Checked := s = '1';
    fs := [];
    If CheckBox1.Checked Then fs := fs + [fsBold];
    If CheckBox2.Checked Then fs := fs + [fsItalic];
    UpdateFont(edit2.text, strtointdef(edit3.text, 12), fs);
    // textborder
    readln(f, s);
    checkbox4.Checked := s = '1';
    CheckBox4Change(Nil);
    readln(f, s);
    edit4.Text := s;
    readln(f, s);
    label5.font.color := stringtoColor(s);
    readln(f, s);
    radiobutton1.Checked := s = '1';
    radiobutton2.Checked := Not radiobutton1.Checked;
    // Underline Options
    readln(f, s);
    edit6.Text := s;
    readln(f, s);
    edit7.Text := s;
    If Not EOF(f) Then Begin
      // Textborder Schwellwert, damits zu alten Versionen Kompatibel bleibt
      readln(f, s);
      ScrollBar1.Position := StrToInt(s);
    End
    Else
      ScrollBar1.Position := 128;
    Schwellwert := ScrollBar1.Position;
    ScrollBar1.Hint := IntToStr(Schwellwert);
    readln(f, s);
    If FileExistsUTF8(s) Then Begin
      LoadOverlay(s);
    End
    Else Begin
      If s <> '' Then Begin
        showmessage('Error unable to locate : ' + LineEnding + s);
      End;
    End;
    readln(f, s);
    If s <> '' Then Begin
      CheckBox3.Checked := odd(StrToInt(s));
    End
    Else Begin
      CheckBox3.Checked := false;
    End;
    closefile(f);
    button2.onclick(Nil);
  End;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Var
  f: Textfile;
Begin
  If SaveDialog2.Execute Then Begin
    SaveDialog2.InitialDir := ExtractFilePath(SaveDialog2.FileName);
    OpenDialog2.InitialDir := ExtractFilePath(SaveDialog2.FileName);
    assignfile(f, SaveDialog2.Filename);
    rewrite(f);
    // Maske
    writeln(f, ButtonMaskFile);
    // Font
    If length(fontfile) <> 0 Then
      writeln(f, fontfile)
    Else
      writeln(f, edit2.Text);
    writeln(f, colortostring(edit2.font.color));
    writeln(f, edit3.Text);
    writeln(f, IntToStr(Ord(checkbox1.Checked)));
    writeln(f, IntToStr(Ord(checkbox2.Checked)));
    // textborder
    writeln(f, IntToStr(Ord(checkbox4.Checked)));
    writeln(f, edit4.Text);
    writeln(f, colortostring(label5.font.color));
    writeln(f, IntToStr(Ord(radiobutton1.Checked)));
    // Underline Options
    writeln(f, edit6.Text);
    writeln(f, edit7.Text);
    // Textborder Schwellwert, damits zu alten Versionen Kompatibel bleibt
    writeln(f, IntToStr(ScrollBar1.Position));
    // Overlaytex
    writeln(f, ButtonOverlayFile);
    writeln(f, IntToStr(Ord(CheckBox3.Checked)));
    closefile(f);
  End;
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Var
  tf: Tfontstyles;
Begin
  tf := ButtonFont.Style;
  If CheckBox1.Checked Then
    include(tf, fsbold)
  Else
    Exclude(tf, fsbold);
  ButtonFont.Style := tf;
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
End;

Procedure TForm1.CheckBox2Change(Sender: TObject);
Var
  tf: Tfontstyles;
Begin
  tf := ButtonFont.Style;
  If CheckBox2.Checked Then
    include(tf, fsItalic)
  Else
    Exclude(tf, fsItalic);
  ButtonFont.Style := tf;
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
End;

Procedure TForm1.CheckBox4Change(Sender: TObject);
Begin
  label4.Enabled := CheckBox4.Checked;
  label5.Enabled := CheckBox4.Checked;
  label9.Enabled := CheckBox4.Checked;
  Edit4.Enabled := CheckBox4.Checked;
  ScrollBar1.Enabled := CheckBox4.Checked;
  Button5.Enabled := CheckBox4.Checked;
  RadioButton1.Enabled := CheckBox4.Checked;
  RadioButton2.Enabled := CheckBox4.Checked;
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
    LoadMask(Opendialog1.filename);
    // Aktualisieren der Ausgabe
    Button2.OnClick(Nil);
  End;
End;

Procedure TForm1.Button10Click(Sender: TObject);
Begin
  If OpenDialog3.Execute Then Begin
    OpenDialog3.InitialDir := ExtractFilePath(OpenDialog3.FileName);
    If Not LoadFontByString(OpenDialog3.FileName) Then Begin
      showmessage('Error unable to load font.');
    End;
    edit2.Text := ExtractFileNameOnly(OpenDialog3.FileName);
    buttonfont.Name := edit2.Text;
    // Aktualisieren der Ausgabe
    Button2.OnClick(Nil);
  End;
End;

Procedure TForm1.Button11Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
    LoadOverlay(Opendialog1.filename);
    // Aktualisieren der Ausgabe
    Button2.OnClick(Nil);
  End;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
End;

Procedure TForm1.Edit3Change(Sender: TObject);
Begin
  ButtonFont.Size := strtointdef(edit3.Text, 0);
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin

  Caption := 'Buttontool ver.: 0.06 by Corpsman support : www.Corpsman.de';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;

  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog2.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog3.InitialDir := ExtractFilePath(ParamStr(0));
  saveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  saveDialog2.InitialDir := ExtractFilePath(ParamStr(0));
  ButtonFont := TFont.Create;
  ButtonFont.Name := 'Sans';
  ButtonFont.Size := 24;
  ButtonFont.Color := clgreen;
  Edit1.Text := 'Button1';
  Edit2.Text := ButtonFont.Name;
  Edit2.Font.Color := ButtonFont.Color;
  Edit3.Text := IntToStr(ButtonFont.Size);
  Edit4.Text := '1';
  Edit5.Text := 'B';
  Edit6.Text := '-4';
  Edit7.Text := '1';
  label5.font.color := clred;
  CheckBox1.Checked := fsBold In ButtonFont.Style;
  CheckBox2.Checked := fsItalic In ButtonFont.Style;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  Schwellwert := ScrollBar1.Position;
  // Aktualisieren der Ausgabe
  Button2.OnClick(Nil);
  ScrollBar1.Hint := IntToStr(Schwellwert);
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  If assigned(ButtonMask) Then
    ButtonMask.Free;
  ButtonMask := Nil;
  If assigned(ResultButton) Then
    ResultButton.Free;
  ResultButton := Nil;
  ButtonFont.Free;
  ButtonFont := Nil;

  If assigned(ButtonOverlay) Then
    ButtonOverlay.Free;
  ButtonOverlay := Nil;
End;

End.

