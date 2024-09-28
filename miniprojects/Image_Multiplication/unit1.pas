(******************************************************************************)
(* Image_Multiplication                                            ??.??.???? *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo that shows the usage of mulimage and foldimage function *)
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
(*               0.02 - Anzeigen diverser Infos                               *)
(*               0.03 - FileDragDrop Support                                  *)
(*               0.04 - add wmFuchsia                                         *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Menus;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses ugraphics, uvectormath, math, Clipbrd;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i, j: Integer;
Begin
  caption := 'Image Matrix Multiplication demo, ver. 0.04';
  //ComboBox3.Items.Add('..' + PathDelim + 'ImageInspector' + PathDelim + 'Images' + PathDelim + 'Bikubik.bmp');
  //ComboBox3.Items.Add('..' + PathDelim + 'ImageInspector' + PathDelim + 'Images' + PathDelim + 'Checkboard.bmp');
  //ComboBox3.Items.Add('..' + PathDelim + 'ImageInspector' + PathDelim + 'Images' + PathDelim + 'Face.bmp');
  //ComboBox3.Items.Add('..' + PathDelim + 'ImageInspector' + PathDelim + 'Images' + PathDelim + 'Tonnen_Verzerrung.bmp');
  ComboBox3.Items.Add('Images' + PathDelim + 'Bikubik.bmp');
  ComboBox3.Items.Add('Images' + PathDelim + 'Checkboard.bmp');
  ComboBox3.Items.Add('Images' + PathDelim + 'Face.bmp');
  ComboBox3.Items.Add('Images' + PathDelim + 'Tonnen_Verzerrung.bmp');
  ComboBox3.ItemIndex := 0;
  label5.caption := format('%0.1f', [0.0]);
  label7.caption := '';
  label10.caption := '';
  For i := 0 To 2 Do Begin
    For j := 0 To 2 Do Begin
      If i = j Then Begin
        StringGrid1.Cells[i, j] := '50'; // 1
      End
      Else Begin
        StringGrid1.Cells[i, j] := '0';
      End;
    End;
  End;
  StringGrid1.Cells[2, 2] := '1';
  Button2.Click;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  If length(FileNames) <> 0 Then Begin
    If lowercase(ExtractFileExt(FileNames[0])) = '.bmp' Then Begin
      ComboBox3.Items.Add(FileNames[0]);
      ComboBox3.ItemIndex := ComboBox3.Items.Count - 1;
      Button2.Click;
    End
    Else Begin
      showmessage('Only .bmp supported.');
    End;
  End;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    ComboBox3.Items.Add(OpenDialog1.FileName);
    ComboBox3.ItemIndex := ComboBox3.Items.Count - 1;
    Button2.Click;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  If SaveDialog1.Execute Then Begin
    b := TBitmap.Create;
    b.Assign(Image1.Picture);
    b.SaveToFile(SaveDialog1.FileName);
    b.free;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  // Save Image to Clipboard
  b := TBitmap.Create;
  b.Assign(Image1.Picture);
  Clipboard.Assign(b);
  b.free;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Var
  s, c: Extended;
Begin
  // Create the rotation matrix by hand ;)
  Label5.Caption := format('%0.1f', [ScrollBar1.Position / 10]);
  sincos(DegToRad(ScrollBar1.Position / 10), s, c);
  StringGrid1.Cells[0, 0] := Format('%0.4f', [c]);
  StringGrid1.Cells[1, 0] := Format('%0.4f', [-s]);
  StringGrid1.Cells[2, 0] := Format('%0.4f', [0.0]);

  StringGrid1.Cells[0, 1] := Format('%0.4f', [s]);
  StringGrid1.Cells[1, 1] := Format('%0.4f', [c]);
  StringGrid1.Cells[2, 1] := Format('%0.4f', [0.0]);

  StringGrid1.Cells[0, 2] := Format('%0.4f', [0.0]);
  StringGrid1.Cells[1, 2] := Format('%0.4f', [0.0]);
  StringGrid1.Cells[2, 2] := Format('%0.4f', [1.0]);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  b: TBitmap;
  m: TMatrix3x3;
  i, j: Integer;
  i64: Int64;
  wm: TWrapMode;
  im: TInterpolationMode;
Begin
  b := TBitmap.Create;
  b.Assign(Image2.Picture.Bitmap);
  For i := 0 To 2 Do Begin
    For j := 0 To 2 Do Begin
      m[i, j] := strtofloatdef(StringGrid1.Cells[i, j], 0);
    End;
  End;
  i64 := GetTickCount64;
  wm := wmBlack;
  Case ComboBox2.ItemIndex Of
    0: wm := wmBlack;
    1: wm := wmFuchsia;
    2: wm := wmClamp;
    3: wm := wmWrap;
  End;
  im := imNone;
  Case ComboBox1.ItemIndex Of
    0: im := imNone;
    1: im := imNearestNeighbour;
    2: im := imBilinear;
    3: im := imCosine;
    4: im := imBicubic;
  End;
  If RadioButton1.Checked Then Begin
    MulImage(b, m, im, wm);
  End
  Else Begin
    foldImage(b, m, CheckBox1.Checked);
  End;
  i64 := GetTickCount64 - i64;
  label7.caption := inttostr(i64) + ' ms';
  image1.Picture.Assign(b);
  label10.Caption := format('%d x %d', [b.Width, b.Height]);
  b.free;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  If Not FileExists(ComboBox3.text) Then exit;
  b := TBitmap.Create;
  b.LoadFromFile(ComboBox3.text);
  Image2.Picture.Assign(b);
  label10.Caption := format('%d x %d', [b.Width, b.Height]);
  b.free;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  Image2.Picture.Assign(Image1.Picture.Bitmap);
End;

End.

