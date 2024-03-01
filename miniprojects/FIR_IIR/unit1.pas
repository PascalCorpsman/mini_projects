(******************************************************************************)
(* FIR_IIR                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo for custom FIR and IIR filter functions                 *)
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
(*               0.02 - Some Improfments in Editing Values                    *)
(*               0.03 - Genmathcalc supports unary -                          *)
(*               0.04 - Add Logging of all Dialog paths in .cfg file          *)
(*                      Some Settings are also logged                         *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  IntfGraphics, fpImage, LCLType, math,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, usolver, inifiles, lazpng, lazjpg;

Type

  { TForm1 }

  TSampleData = Record
    x: TDatatype; // Damit beim Anzeigen entschieden werden kann welche werte wie sichtbar sind ..
    y: TDatatype;
  End;

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    Edit23: TEdit;
    Edit26: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenDialog3: TOpenDialog;
    OpenDialog4: TOpenDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    SaveDialog4: TSaveDialog;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Button14Click(Sender: TObject);
    Procedure Button15Click(Sender: TObject);
    Procedure Button16Click(Sender: TObject);
    Procedure Button17Click(Sender: TObject);
    Procedure Button18Click(Sender: TObject);
    Procedure Button19Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button20Click(Sender: TObject);
    Procedure Button21Click(Sender: TObject);
    Procedure Button22Click(Sender: TObject);
    Procedure Button23Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure Edit3KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure RadioGroup3Click(Sender: TObject);
    Procedure RadioGroup4Click(Sender: TObject);
    Procedure StringGrid1KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure StringGrid2KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    sample_cnt: integer;
    Samples: Array Of TSampleData;
    results: Array Of TSampleData;
    Daten_D2: TBitmap;
    Procedure SaveSample(x, y: TDatatype);
    Procedure ClearAllSamples();
    Procedure CalcFilter();
    Procedure CalcFilter2D();
    Function get_Cn(index: integer; out_of_range: integer; out_of_range_value: TDatatype): TDatatype;
    Function get_Dn(index: integer; out_of_range: integer; out_of_range_value: TDatatype): TDatatype;
    Function get_fir(index: integer): TDatatype;
    Function get_iir(index: integer): TDatatype;
  End;

  TFIR = Record
    K, L: integer;
    C: Array Of TDatatype;
    out_of_range: integer;
    out_of_range_value: TDatatype;
    cs: TDatatype;
  End;

  TIIR = Record
    M: integer;
    D: Array Of TDatatype;
    out_of_range: integer;
    out_of_range_value: TDatatype;
    ds: TDatatype;
  End;

Var
  Form1: TForm1;
  Solver: TSolver;
  fir: TFIR;
  iir: TIIR;
  ini: Tinifile;

Implementation

{$R *.lfm}

Uses unit2, unit3, unit4, unit5, lazutf8, LazFileUtils;

{ TForm1 }

Function TForm1.get_Cn(index: integer; out_of_range: integer;
  out_of_range_value: TDatatype): TDatatype;
Begin
  If (index < 0) Or (index >= sample_cnt) Then Begin
    result := 0; // Init, darf aber eigentlich nicht vorkommen, da die Case Erschöpfend sein sollte
    Case out_of_range Of
      0: Begin // Clamp
          If index < 0 Then
            result := Samples[0].y
          Else
            result := Samples[high(Samples)].y;
        End;
      1: Begin // Repeat
          While index < 0 Do
            index := index + sample_cnt;
          While index >= sample_cnt Do
            index := index - sample_cnt;
          result := Samples[index].y;
        End;
      2: Begin // 0
          result := 0;
        End;
      3: Begin // Value
          result := out_of_range_value;
        End;
    End;
  End
  Else Begin
    result := Samples[index].y;
  End;
End;

Function TForm1.get_Dn(index: integer; out_of_range: integer;
  out_of_range_value: TDatatype): TDatatype;
Begin
  If (index < 0) Then Begin
    result := 0; // Init, darf aber eigentlich nicht vorkommen, da die Case Erschöpfend sein sollte
    Case out_of_range Of
      0: Begin // Clamp
          result := results[0].y
        End;
      1: Begin // 0
          result := 0;
        End;
      2: Begin // Value
          result := out_of_range_value;
        End;
    End;
  End
  Else Begin
    result := results[index].y;
  End;
End;

Function TForm1.get_fir(index: integer): TDatatype;
Var
  i: Integer;
  s: TDatatype;
Begin
  s := 0;
  // die Vergangenen
  For i := 0 To fir.K - 1 Do Begin
    s := s + get_Cn(index + i - fir.K, fir.out_of_range, fir.out_of_range_value) * fir.C[i];
  End;
  // Der Aktuelle
  s := s + get_Cn(index, fir.out_of_range, fir.out_of_range_value) * fir.C[fir.k];
  // Die Zukünftigen
  For i := 0 To fir.L - 1 Do Begin
    s := s + get_Cn(index + i - fir.l + fir.K + 1, fir.out_of_range, fir.out_of_range_value) * fir.C[i + fir.K + 1];
  End;
  // Einrechnen der Gesamt Skallierung
  s := s * fir.cs;
  // Fertig
  result := s;
End;

Function TForm1.get_iir(index: integer): TDatatype;
Var
  i: Integer;
  s: TDatatype;
Begin
  s := 0;
  // die Vergangenen
  For i := 0 To iir.M - 1 Do Begin
    s := s + get_dn(index + i - iir.m - 1, iir.out_of_range, iir.out_of_range_value) * iir.D[i];
  End;
  // Einrechnen der Gesamt Skallierung
  s := s * iir.ds;
  // Fertig
  result := s;
End;

Procedure TForm1.CalcFilter;
Var
  i: integer;
  iir_, fir_: TDatatype;
Begin
  setlength(results, sample_cnt);
  For i := 0 To sample_cnt - 1 Do Begin
    results[i].x := samples[i].x;
    fir_ := get_fir(i);
    iir_ := get_iir(i);
    results[i].y := fir_ + iir_;
  End;
End;

Function GetRValue(Color: TFPColor): Byte;
Begin
  result := Color.red Shr 8;
End;

Function GetGValue(Color: TFPColor): Byte;
Begin
  result := Color.green Shr 8;
End;

Function GetBValue(Color: TFPColor): Byte;
Begin
  result := Color.blue Shr 8;
End;

Function RGB(r, g, b: Byte): TFPColor;
Var
  tmp: integer;
Begin
  tmp := r;
  result.red := (tmp Shl 8) Or $FF;
  tmp := g;
  result.green := (tmp Shl 8) Or $FF;
  tmp := b;
  result.blue := (tmp Shl 8) Or $FF;
  result.alpha := 0;
End;

Function Luminance(C: TFPColor): Byte;
Begin
  //   Y = 0.3R + 0.59G + 0.11B
  result := min(255, max(0, round(0.3 * (c.red Shr 8) + 0.59 * (c.green Shr 8) + 0.11 * (c.blue Shr 8))));
End;

Procedure TForm1.CalcFilter2D;
Var
  TempIntfImg_X: TLazIntfImage;

  Function get_pixel_X(x, y: integer; out_of_range: integer;
    out_of_range_value: TDatatype): TFPColor;
  Var
    tmp: byte;
  Begin
    If (x < 0) Or (x >= Daten_D2.Width) Or
      (y < 0) Or (y >= Daten_D2.Height) Then Begin
      result.alpha := 0;
      result.blue := 0;
      result.green := 0;
      result.red := 0;
      Case out_of_range Of
        0: Begin // Clamp
            If x < 0 Then x := 0;
            If x >= Daten_D2.Width Then x := Daten_D2.Width - 1;
            If y < 0 Then y := 0;
            If y >= Daten_D2.Height Then y := Daten_D2.Height - 1;
            result := TempIntfImg_X.Colors[x, y];
          End;
        1: Begin // Repeat
            While x < 0 Do
              x := x + Daten_D2.Width;
            While x >= Daten_D2.Width Do
              x := x - Daten_D2.Width;
            While y < 0 Do
              y := y + Daten_D2.Height;
            While y >= Daten_D2.Height Do
              y := y - Daten_D2.Height;
            result := TempIntfImg_X.Colors[x, y];
          End;
        2: Begin // 0
            // Nichts zu tun, da Result bereits mit 0 Initialisiert ist
          End;
        3: Begin // Value
            tmp := min(255, max(0, round(out_of_range_value)));
            result := RGB(tmp, tmp, tmp);
          End;
      End;
    End
    Else Begin
      result := TempIntfImg_X.Colors[x, y];
    End;
  End;

Var
  y_Data: TBitmap;
  TempIntfImg_Y: TLazIntfImage;

  Function get_pixel_Y(x, y: integer; out_of_range: integer;
    out_of_range_value: TDatatype): TFPColor;
  Var
    tmp: byte;
  Begin
    If (x < 0) Or (x >= Daten_D2.Width) Or
      (y < 0) Or (y >= Daten_D2.Height) Then Begin
      result.alpha := 0;
      result.blue := 0;
      result.green := 0;
      result.red := 0;
      Case out_of_range Of
        0: Begin // Clamp
            If x < 0 Then x := 0;
            If x >= Daten_D2.Width Then x := Daten_D2.Width - 1;
            If y < 0 Then y := 0;
            If y >= Daten_D2.Height Then y := Daten_D2.Height - 1;
            result := TempIntfImg_Y.Colors[x, y];
          End;
        1: Begin // 0
            // Nichts zu tun, da Result bereits mit 0 Initialisiert ist
          End;
        2: Begin // Value
            tmp := min(255, max(0, round(out_of_range_value)));
            result := RGB(tmp, tmp, tmp);
          End;
      End;
    End
    Else Begin
      result := TempIntfImg_Y.Colors[x, y];
    End;
  End;

Var
  i, j, k, l: integer;
  ImgHandle, ImgMaskHandle: HBitmap;
  fir_data_R: TDatatype;
  fir_data_G: TDatatype;
  fir_data_B: TDatatype;
  iir_data_R: TDatatype;
  iir_data_G: TDatatype;
  iir_data_B: TDatatype;

  fir_data_R2: TDatatype;
  fir_data_G2: TDatatype;
  fir_data_B2: TDatatype;
  iir_data_R2: TDatatype;
  iir_data_G2: TDatatype;
  iir_data_B2: TDatatype;

  a_Color: TFPColor;
  gray_sace_res: Boolean;
  r, g, b: byte;
  Fir_2d: Array Of Array Of TDatatype;
  iir_2d: Array Of Array Of TDatatype;

Begin
  TempIntfImg_X := TLazIntfImage.Create(0, 0);
  TempIntfImg_X.LoadFromBitmap(Daten_D2.Handle, Daten_D2.MaskHandle);
  gray_sace_res := false;
  If (pagecontrol3.PageIndex = 0) And (RadioGroup4.ItemIndex = 8) Then
    gray_sace_res := true;
  y_data := TBitmap.create;
  y_data.Width := Daten_D2.Width;
  y_data.Height := Daten_D2.Height;
  TempIntfImg_Y := TLazIntfImage.Create(0, 0);
  TempIntfImg_Y.LoadFromBitmap(y_data.Handle, y_data.MaskHandle);
  If Not CheckBox8.Checked And Not CheckBox9.Checked Then Begin
    showmessage('Error no filter direction selected, the result image will be black.');
  End;
  // Nur X-Richtung
  If CheckBox8.Checked And Not CheckBox9.Checked Then Begin
    For j := 0 To Daten_D2.Height - 1 Do Begin
      For i := 0 To Daten_D2.Width - 1 Do Begin
        // FIR - Daten ---------------------------------------------------------
        fir_data_R := 0;
        fir_data_G := 0;
        fir_data_B := 0;
        For k := 0 To fir.K - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.K, fir.out_of_range, fir.out_of_range_value) * fir.C[i];
          a_Color := get_pixel_X(i + k - fir.K, j, fir.out_of_range, fir.out_of_range_value);
          fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[k];
          fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[k];
          fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[k];
        End;
        // Der Aktuelle
        //  s := s + get_Cn(index, fir.out_of_range, fir.out_of_range_value) * fir.C[fir.k];
        a_Color := get_pixel_X(i, j, fir.out_of_range, fir.out_of_range_value);
        fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[fir.k];
        fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[fir.k];
        fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[fir.k];
        // Die Zukünftigen
        For k := 0 To fir.L - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.l + fir.K + 1, fir.out_of_range, fir.out_of_range_value) * fir.C[i + fir.K + 1];
          a_Color := get_pixel_X(i + k - fir.l + fir.k + 1, j, fir.out_of_range, fir.out_of_range_value);
          fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[k + fir.K + 1];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * fir.cs;
        fir_data_R := fir_data_R * fir.cs;
        fir_data_G := fir_data_G * fir.cs;
        fir_data_B := fir_data_B * fir.cs;
        // FIR - Daten ---------------------------------------------------------
        iir_data_R := 0;
        iir_data_G := 0;
        iir_data_B := 0;
        // die Vergangenen
        For k := 0 To iir.M - 1 Do Begin
          //  s := s + get_dn(index + i - iir.m - 1, iir.out_of_range, iir.out_of_range_value) * iir.D[i];
          a_Color := get_pixel_Y(i + k - iir.m - 1, j, iir.out_of_range, iir.out_of_range_value);
          iir_data_R := iir_data_R + GetRValue(a_color) * iir.D[k];
          iir_data_G := iir_data_G + GetGValue(a_color) * iir.D[k];
          iir_data_B := iir_data_B + GetBValue(a_color) * iir.D[k];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * iir.ds;
        iir_data_R := iir_data_R * iir.ds;
        iir_data_G := iir_data_G * iir.ds;
        iir_data_B := iir_data_B * iir.ds;
        // Ausgabe -------------------------------------------------------------
        r := min(255, max(0, round(abs(fir_data_R + iir_data_R))));
        g := min(255, max(0, round(abs(fir_data_g + iir_data_g))));
        b := min(255, max(0, round(abs(fir_data_b + iir_data_b))));
        a_Color := RGB(r, g, b);
        // Umrechnen in Graustufen, wenn Notwendig
        If gray_sace_res Then Begin
          r := Luminance(a_Color);
          a_Color := RGB(r, r, r);
        End;
        TempIntfImg_Y.Colors[i, j] := a_Color;
      End;
    End;
  End;
  // Nur Y-Richtung
  If Not CheckBox8.Checked And CheckBox9.Checked Then Begin
    For j := 0 To Daten_D2.Height - 1 Do Begin
      For i := 0 To Daten_D2.Width - 1 Do Begin
        // FIR - Daten ---------------------------------------------------------
        fir_data_R := 0;
        fir_data_G := 0;
        fir_data_B := 0;
        For k := 0 To fir.K - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.K, fir.out_of_range, fir.out_of_range_value) * fir.C[i];
          a_Color := get_pixel_X(i, j + k - fir.K, fir.out_of_range, fir.out_of_range_value);
          fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[k];
          fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[k];
          fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[k];
        End;
        // Der Aktuelle
        //  s := s + get_Cn(index, fir.out_of_range, fir.out_of_range_value) * fir.C[fir.k];
        a_Color := get_pixel_X(i, j, fir.out_of_range, fir.out_of_range_value);
        fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[fir.k];
        fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[fir.k];
        fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[fir.k];
        // Die Zukünftigen
        For k := 0 To fir.L - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.l + fir.K + 1, fir.out_of_range, fir.out_of_range_value) * fir.C[i + fir.K + 1];
          a_Color := get_pixel_X(i, j + k - fir.l + fir.k + 1, fir.out_of_range, fir.out_of_range_value);
          fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[k + fir.K + 1];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * fir.cs;
        fir_data_R := fir_data_R * fir.cs;
        fir_data_G := fir_data_G * fir.cs;
        fir_data_B := fir_data_B * fir.cs;
        // FIR - Daten ---------------------------------------------------------
        iir_data_R := 0;
        iir_data_G := 0;
        iir_data_B := 0;
        // die Vergangenen
        For k := 0 To iir.M - 1 Do Begin
          //  s := s + get_dn(index + i - iir.m - 1, iir.out_of_range, iir.out_of_range_value) * iir.D[i];
          a_Color := get_pixel_Y(i, j + k - iir.m - 1, iir.out_of_range, iir.out_of_range_value);
          iir_data_R := iir_data_R + GetRValue(a_color) * iir.D[k];
          iir_data_G := iir_data_G + GetGValue(a_color) * iir.D[k];
          iir_data_B := iir_data_B + GetBValue(a_color) * iir.D[k];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * iir.ds;
        iir_data_R := iir_data_R * iir.ds;
        iir_data_G := iir_data_G * iir.ds;
        iir_data_B := iir_data_B * iir.ds;
        // Ausgabe -------------------------------------------------------------
        r := min(255, max(0, round(abs(fir_data_R + iir_data_R))));
        g := min(255, max(0, round(abs(fir_data_g + iir_data_g))));
        b := min(255, max(0, round(abs(fir_data_b + iir_data_b))));
        a_Color := RGB(r, g, b);
        // Umrechnen in Graustufen, wenn Notwendig
        If gray_sace_res Then Begin
          r := Luminance(a_Color);
          a_Color := RGB(r, r, r);
        End;
        TempIntfImg_Y.Colors[i, j] := a_Color;
      End;
    End;
  End;
  // Nur Beide Richtungen
  If CheckBox8.Checked And CheckBox9.Checked And RadioButton6.Checked Then Begin
    For j := 0 To Daten_D2.Height - 1 Do Begin
      For i := 0 To Daten_D2.Width - 1 Do Begin
        // FIR - Daten ---------------------------------------------------------
        fir_data_R := 0;
        fir_data_G := 0;
        fir_data_B := 0;
        For k := 0 To fir.K - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.K, fir.out_of_range, fir.out_of_range_value) * fir.C[i];
          a_Color := get_pixel_X(i + k - fir.K, j, fir.out_of_range, fir.out_of_range_value);
          fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[k];
          fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[k];
          fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[k];
        End;
        // Der Aktuelle
        //  s := s + get_Cn(index, fir.out_of_range, fir.out_of_range_value) * fir.C[fir.k];
        a_Color := get_pixel_X(i, j, fir.out_of_range, fir.out_of_range_value);
        fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[fir.k];
        fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[fir.k];
        fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[fir.k];
        // Die Zukünftigen
        For k := 0 To fir.L - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.l + fir.K + 1, fir.out_of_range, fir.out_of_range_value) * fir.C[i + fir.K + 1];
          a_Color := get_pixel_X(i + k - fir.l + fir.k + 1, j, fir.out_of_range, fir.out_of_range_value);
          fir_data_R := fir_data_R + GetRValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_G := fir_data_G + GetGValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_B := fir_data_B + GetBValue(a_color) * fir.C[k + fir.K + 1];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * fir.cs;
        fir_data_R := fir_data_R * fir.cs;
        fir_data_G := fir_data_G * fir.cs;
        fir_data_B := fir_data_B * fir.cs;
        // FIR - Daten ---------------------------------------------------------
        iir_data_R := 0;
        iir_data_G := 0;
        iir_data_B := 0;
        // die Vergangenen
        For k := 0 To iir.M - 1 Do Begin
          //  s := s + get_dn(index + i - iir.m - 1, iir.out_of_range, iir.out_of_range_value) * iir.D[i];
          a_Color := get_pixel_Y(i + k - iir.m - 1, j, iir.out_of_range, iir.out_of_range_value);
          iir_data_R := iir_data_R + GetRValue(a_color) * iir.D[k];
          iir_data_G := iir_data_G + GetGValue(a_color) * iir.D[k];
          iir_data_B := iir_data_B + GetBValue(a_color) * iir.D[k];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * iir.ds;
        iir_data_R := iir_data_R * iir.ds;
        iir_data_G := iir_data_G * iir.ds;
        iir_data_B := iir_data_B * iir.ds;
        // FIR - Daten ---------------------------------------------------------
        fir_data_R2 := 0;
        fir_data_G2 := 0;
        fir_data_B2 := 0;
        For k := 0 To fir.K - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.K, fir.out_of_range, fir.out_of_range_value) * fir.C[i];
          a_Color := get_pixel_X(i, j + k - fir.K, fir.out_of_range, fir.out_of_range_value);
          fir_data_R2 := fir_data_R2 + GetRValue(a_color) * fir.C[k];
          fir_data_G2 := fir_data_G2 + GetGValue(a_color) * fir.C[k];
          fir_data_B2 := fir_data_B2 + GetBValue(a_color) * fir.C[k];
        End;
        // Der Aktuelle
        //  s := s + get_Cn(index, fir.out_of_range, fir.out_of_range_value) * fir.C[fir.k];
        a_Color := get_pixel_X(i, j, fir.out_of_range, fir.out_of_range_value);
        fir_data_R2 := fir_data_R2 + GetRValue(a_color) * fir.C[fir.k];
        fir_data_G2 := fir_data_G2 + GetGValue(a_color) * fir.C[fir.k];
        fir_data_B2 := fir_data_B2 + GetBValue(a_color) * fir.C[fir.k];
        // Die Zukünftigen
        For k := 0 To fir.L - 1 Do Begin
          //  s := s + get_Cn(index + i - fir.l + fir.K + 1, fir.out_of_range, fir.out_of_range_value) * fir.C[i + fir.K + 1];
          a_Color := get_pixel_X(i, j + k - fir.l + fir.k + 1, fir.out_of_range, fir.out_of_range_value);
          fir_data_R2 := fir_data_R2 + GetRValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_G2 := fir_data_G2 + GetGValue(a_color) * fir.C[k + fir.K + 1];
          fir_data_B2 := fir_data_B2 + GetBValue(a_color) * fir.C[k + fir.K + 1];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * fir.cs;
        fir_data_R2 := fir_data_R2 * fir.cs;
        fir_data_G2 := fir_data_G2 * fir.cs;
        fir_data_B2 := fir_data_B2 * fir.cs;
        // FIR - Daten ---------------------------------------------------------
        iir_data_R2 := 0;
        iir_data_G2 := 0;
        iir_data_B2 := 0;
        // die Vergangenen
        For k := 0 To iir.M - 1 Do Begin
          //  s := s + get_dn(index + i - iir.m - 1, iir.out_of_range, iir.out_of_range_value) * iir.D[i];
          a_Color := get_pixel_Y(i, j + k - iir.m - 1, iir.out_of_range, iir.out_of_range_value);
          iir_data_R2 := iir_data_R2 + GetRValue(a_color) * iir.D[k];
          iir_data_G2 := iir_data_G2 + GetGValue(a_color) * iir.D[k];
          iir_data_B2 := iir_data_B2 + GetBValue(a_color) * iir.D[k];
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * iir.ds;
        iir_data_R2 := iir_data_R2 * iir.ds;
        iir_data_G2 := iir_data_G2 * iir.ds;
        iir_data_B2 := iir_data_B2 * iir.ds;

        fir_data_R := (fir_data_R + fir_data_R2) / 2;
        fir_data_G := (fir_data_G + fir_data_G2) / 2;
        fir_data_B := (fir_data_B + fir_data_B2) / 2;

        // Ausgabe -------------------------------------------------------------
        r := min(255, max(0, round(abs(fir_data_R + iir_data_R))));
        g := min(255, max(0, round(abs(fir_data_g + iir_data_g))));
        b := min(255, max(0, round(abs(fir_data_b + iir_data_b))));
        a_Color := RGB(r, g, b);
        // Umrechnen in Graustufen, wenn Notwendig
        If gray_sace_res Then Begin
          r := Luminance(a_Color);
          a_Color := RGB(r, r, r);
        End;
        TempIntfImg_Y.Colors[i, j] := a_Color;
      End;
    End;
  End;
  If CheckBox8.Checked And CheckBox9.Checked And RadioButton5.Checked Then Begin
    // Würden wir die beiden Operationen einfach nach einander ausführen käme murks raus
    // da Hochpassfilter dann die Orthogonale Frequenzen bereits eleminiert hätten
    // Korrekt gefiltert wird, wenn der Filter mit sich selbst gefaltet wird => 2D-Filter
    // 1. Erzeugen der 2D-Filter
    fir_2d := Nil;
    setlength(fir_2d, fir.K + fir.L + 1, fir.K + fir.L + 1);
    For i := 0 To high(Fir_2d) Do Begin
      For j := 0 To high(Fir_2d) Do Begin
        fir_2d[i, j] := fir.C[i] * fir.C[j];
      End;
    End;
    iir_2d := Nil;
    setlength(iir_2d, iir.M, iir.M);
    For i := 0 To high(iir_2d) Do Begin
      For j := 0 To high(iir_2d) Do Begin
        iir_2d[i, j] := iir.D[i] * iir.D[j];
      End;
    End;
    // 2. Anwenden der 2D-Filtermaske
    For j := 0 To Daten_D2.Height - 1 Do Begin
      For i := 0 To Daten_D2.Width - 1 Do Begin
        // FIR - Daten ---------------------------------------------------------
        fir_data_R := 0;
        fir_data_G := 0;
        fir_data_B := 0;
        // Jeden Pixel Falten mit dem 2D-Filter
        For l := 0 To high(fir_2d) Do Begin
          For k := 0 To high(fir_2d) Do Begin
            //  s := s + get_Cn(index + i - fir.K, fir.out_of_range, fir.out_of_range_value) * fir.C[i];
            a_Color := get_pixel_X(i + k - fir.K, j + l - fir.K, fir.out_of_range, fir.out_of_range_value);
            fir_data_R := fir_data_R + GetRValue(a_color) * fir_2d[k, l];
            fir_data_G := fir_data_G + GetGValue(a_color) * fir_2d[k, l];
            fir_data_B := fir_data_B + GetBValue(a_color) * fir_2d[k, l];
          End;
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * fir.cs;
        fir_data_R := fir_data_R * fir.cs * fir.cs;
        fir_data_G := fir_data_G * fir.cs * fir.cs;
        fir_data_B := fir_data_B * fir.cs * fir.cs;
        // FIR - Daten ---------------------------------------------------------
        iir_data_R := 0;
        iir_data_G := 0;
        iir_data_B := 0;
        // Jeden Pixel Falten mit dem 2D-Filter
        For l := 0 To high(iir_2d) Do Begin
          For k := 0 To high(iir_2d) Do Begin
            //  s := s + get_dn(index + i - iir.m - 1, iir.out_of_range, iir.out_of_range_value) * iir.D[i];
            a_Color := get_pixel_Y(i + k - iir.m - 1, j + l - iir.m - 1, iir.out_of_range, iir.out_of_range_value);
            iir_data_r := iir_data_r + GetRValue(a_color) * iir_2d[k, l];
            iir_data_G := iir_data_G + GetGValue(a_color) * iir_2d[k, l];
            iir_data_B := iir_data_B + GetBValue(a_color) * iir_2d[k, l];
          End;
        End;
        // Einrechnen der Gesamt Skallierung
        //  s := s * iir.ds;
        iir_data_R := iir_data_R * iir.ds * iir.ds;
        iir_data_G := iir_data_G * iir.ds * iir.ds;
        iir_data_B := iir_data_B * iir.ds * iir.ds;
        // Ausgabe -------------------------------------------------------------
        r := min(255, max(0, round(abs(fir_data_R + iir_data_R))));
        g := min(255, max(0, round(abs(fir_data_g + iir_data_g))));
        b := min(255, max(0, round(abs(fir_data_b + iir_data_b))));
        a_Color := RGB(r, g, b);
        // Umrechnen in Graustufen, wenn Notwendig
        If gray_sace_res Then Begin
          r := Luminance(a_Color);
          a_Color := RGB(r, r, r);
        End;
        TempIntfImg_Y.Colors[i, j] := a_Color;
      End;
    End;
  End;
  TempIntfImg_Y.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  y_data.Handle := ImgHandle;
  y_data.MaskHandle := ImgMaskHandle;
  Daten_D2.Canvas.Draw(0, 0, y_data);
  y_Data.free;
  TempIntfImg_Y.Free;
  TempIntfImg_X.free;
End;

(*
 * Return Value -1 = Little Endian style
 * Return Value 0  = Unknown Endian style
 * Return Value 1  = Big Endian style
 *)

Function get_endian_style: integer;
Type
  Q = Record
    Case boolean Of
      true: (i: integer);
      false: (p: Array[1..4] Of byte)
  End;
Var
  x: ^Q;
Begin
  result := 0;
  new(x);
  x^.i := 5;
  If x^.p[1] = 5 Then
    result := -1
  Else If x^.p[4] = 5 Then
    result := 1
  Else
    result := 0;
  dispose(x);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  dummy: word;
Begin
  // Init Global
{$IFDEF Windows}
  button11.caption := 'select';
  button12.caption := 'select';
{$ELSE}
  button11.caption := 'enhance' + LineEnding + '  color';
  button12.caption := 'mark' + LineEnding + 'color';
{$ENDIF}
  Caption := 'FIR / IIR Sandbox ver. 0.04 by Corpsman, support : www.Corpsman.de';
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  defFormat := DefaultFormatSettings;
  defFormat.DecimalSeparator := '.';
  solver := Tsolver.create;
  setlength(Samples, 0);
  Daten_D2 := TBitmap.Create;
  PageControl1.TabIndex := 0;
  PageControl2.TabIndex := 1;
  PageControl3.TabIndex := 1;
  ini := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'fii_iir.cfg');
  OpenDialog1.InitialDir := ini.ReadString('General', 'OD1', ExtractFilePath(paramstr(0)));
  SaveDialog1.InitialDir := ini.ReadString('General', 'SD1', ExtractFilePath(paramstr(0)));
  OpenDialog2.InitialDir := ini.ReadString('General', 'OD2', ExtractFilePath(paramstr(0)));
  SaveDialog2.InitialDir := ini.ReadString('General', 'SD2', ExtractFilePath(paramstr(0)));
  OpenDialog3.InitialDir := ini.ReadString('General', 'OD3', ExtractFilePath(paramstr(0)));
  SaveDialog3.InitialDir := ini.ReadString('General', 'SD3', ExtractFilePath(paramstr(0)));
  OpenDialog4.InitialDir := ini.ReadString('General', 'OD4', ExtractFilePath(paramstr(0)));
  SaveDialog4.InitialDir := ini.ReadString('General', 'SD4', ExtractFilePath(paramstr(0)));
  // Init Input Data
  Edit1.text := '0.0';
  edit2.text := '0.100';
  edit3.text := '0.00025';
  Memo1.Text :=
    '// This is a comment' + LineEnding +
    '{ This is a comment' + LineEnding +
    '   over two lines }' + LineEnding +
    '// Create a 50 Hz Sinus, with 25 as mean value' + LineEnding +
    '// Simulated for 5 periods ( = 0.100 sec.), sampled each 250 nano seconds' + LineEnding +
    'sqrt(2) * 25 * sin(2 * Pi * x * 50) // This signal will be filtered out by the highpass filter' + LineEnding +
    '// Add some noise at 500 Hz and amplitude 8, around the 50 ms mark' + LineEnding +
    '+ 8 * sin(2 * Pi * x * 500) *( e ^ ( -((x - 0.050)^2) * 1500 {attenuation}))';
  label21.caption := '';
  // Input Data Files
  Edit21.text := '0.0';
  Edit22.text := '';
  edit23.text := ini.readstring('Input_Data', 'file_delta', '0.00025');
  RadioGroup3.ItemIndex := 6;
  // Init Filter
  edit12.text := '2';
  edit13.text := '2';
  button17.OnClick(Nil);
  StringGrid1.Cells[1, 1] := '1';
  StringGrid1.Cells[1, 2] := '-4';
  StringGrid1.Cells[1, 3] := '6';
  StringGrid1.Cells[1, 4] := '-4';
  StringGrid1.Cells[1, 5] := '1';
  StringGrid1.ColWidths[1] := 94;
  RadioGroup1.ItemIndex := 1;
  edit16.text := '0.0';
  edit14.text := '3.2';
  dummy := 0;
  StringGrid1KeyUp(Nil, dummy, []); // Aktualisieren der summe cn
  edit17.text := '0';
  button18.OnClick(Nil);
  StringGrid2KeyUp(Nil, dummy, []); // Aktualisieren der summe cn
  StringGrid2.ColWidths[1] := 94;
  RadioGroup2.ItemIndex := 1;
  edit18.text := '0.0';
  edit19.text := '0.0';
  label20.caption := '';
  // Init Output Data
  edit6.text := '-0.005';
  edit7.text := '0.105';
  edit4.Text := '-60';
  edit5.Text := '60';
  CheckBox1.Checked := true;
  image1.Color := clgray;
  image2.Color := clred;
  image3.Color := clsilver;
  image4.Color := clblack;
  CheckBox2.Checked := true;
  CheckBox3.Checked := true;
  CheckBox4.Checked := true;
  CheckBox5.Checked := true;
  edit8.text := '0.005';
  edit9.text := '5';
  edit10.text := '0.01';
  edit11.text := '10';
  label22.caption := '';
  Edit26.text := '';
  RadioGroup4.ItemIndex := 6;
  // Anzeigen in welcher "Endian" Kodierung die Daten erwartet werden
  Case get_endian_style Of
    -1: Begin // Little
        label25.caption := 'The data inside the files is' + LineEnding + 'required as "Little Endian"';
        label27.caption := 'The data inside the output files' + LineEnding + 'will be in "Little Endian"';
      End;
    0: Begin // Unbekannt
        label25.caption := 'The Endian type of your machine' + LineEnding + 'could not be detected.';
        label27.caption := 'The data inside the output files' + LineEnding + 'will be in an unknown Endian style';
      End;
    1: Begin // Big
        label25.caption := 'The data inside the files is' + LineEnding + 'required as "Big Endian"';
        label27.caption := 'The data inside the output files' + LineEnding + 'will be in "Big Endian"';
      End;
  End;

End;

Procedure TForm1.PageControl1Change(Sender: TObject);
Begin
  Image1.Canvas.Brush.Color := Image1.Color;
  Image1.Canvas.Brush.Style := bsSolid;
  Image1.Canvas.Rectangle(-1, -1, Image1.Width + 1, Image1.Height + 1);
  Image2.Canvas.Brush.Color := Image2.Color;
  Image2.Canvas.Brush.Style := bsSolid;
  Image2.Canvas.Rectangle(-1, -1, Image2.Width + 1, Image2.Height + 1);
  Image3.Canvas.Brush.Color := Image3.Color;
  Image3.Canvas.Brush.Style := bsSolid;
  Image3.Canvas.Rectangle(-1, -1, Image3.Width + 1, Image3.Height + 1);
  Image4.Canvas.Brush.Color := Image4.Color;
  Image4.Canvas.Brush.Style := bsSolid;
  Image4.Canvas.Rectangle(-1, -1, Image4.Width + 1, Image4.Height + 1);
End;

Procedure TForm1.RadioGroup3Click(Sender: TObject);
Begin
  label19.enabled := (RadioGroup3.ItemIndex = 8) Or (RadioGroup3.ItemIndex = 9);
  CheckBox8.Enabled := label19.enabled;
  CheckBox9.Enabled := label19.enabled;
  label23.enabled := label19.enabled;
  RadioButton5.enabled := label19.enabled And CheckBox8.Checked And CheckBox9.Checked;
  RadioButton6.enabled := label19.enabled And CheckBox8.Checked And CheckBox9.Checked;
  label25.enabled := Not label19.enabled;
End;

Procedure TForm1.RadioGroup4Click(Sender: TObject);
Begin
  label24.enabled := (RadioGroup4.ItemIndex = 8) Or (RadioGroup4.ItemIndex = 9);
  label27.enabled := Not label24.enabled;
End;

Procedure TForm1.StringGrid1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  i: integer;
  f: Single;
Begin
  f := 0;
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    f := f + StrToFloatDef(StringGrid1.Cells[1, i], 0, defFormat);
  End;
  edit15.text := floattostr(f, defFormat);
End;

Procedure TForm1.StringGrid2KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  i: integer;
  f: Single;
Begin
  f := 0;
  For i := 1 To StringGrid2.RowCount - 1 Do Begin
    f := f + StrToFloatDef(StringGrid2.Cells[1, i], 0, defFormat);
  End;
  edit20.text := floattostr(f, defFormat);
End;

// Ein Eingelesener Datensatz

Procedure TForm1.SaveSample(x, y: TDatatype);
Begin
  // Blockweises Allokieren der Daten immer in 1 KB - Blöcken
  If (sample_cnt) > high(samples) Then Begin
    setlength(Samples, high(Samples) + 1025);
  End;
  // Hinzufügen des Samples
  samples[sample_cnt].x := x;
  samples[sample_cnt].y := y;
  sample_cnt := sample_cnt + 1;
End;

Procedure TForm1.ClearAllSamples;
Begin
  sample_cnt := 0;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  solver.free;
  setlength(Samples, 0);
  setlength(results, 0);
  setlength(fir.C, 0);
  setlength(iir.D, 0);
  Daten_D2.free;
  ini.WriteString('General', 'OD1', OpenDialog1.InitialDir);
  ini.WriteString('General', 'SD1', SaveDialog1.InitialDir);
  ini.WriteString('General', 'OD2', OpenDialog2.InitialDir);
  ini.WriteString('General', 'SD2', SaveDialog2.InitialDir);
  ini.WriteString('General', 'OD3', OpenDialog3.InitialDir);
  ini.WriteString('General', 'SD3', SaveDialog3.InitialDir);
  ini.WriteString('General', 'OD4', OpenDialog4.InitialDir);
  ini.WriteString('General', 'SD4', SaveDialog4.InitialDir);
  ini.WriteString('Input_Data', 'file_delta', edit23.text);
  ini.free;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  y, x, StartVal, EndVal, DeltaVal: TDatatype;
  i: integer;
  f: TFileStream;
  // Datentyp behälter
  blocksize: integer;
  type_0: Byte; //                            0 to 255
  type_1: ShortInt; //                     -127 to 127
  type_2: Word; //                            0 to 65,535
  type_3: SmallInt; //                  -32,768 to 32,767
  type_4: LongWord; //                        0 to 4,294,967,295
  type_5: Integer; //            -2,147,483,648 to 2,147,483,647
  type_6: Single; //      7  significant digits, exponent   -38 to +38
  //  type_7: Double; //     15  significant digits, exponent  -308 to +308
  res_f_name: String;
  png: TPNGImage;
  jpg: TJPGImage;
Begin
  // Löschen der Alten Daten ( Also eigentlich wird nur der Endpointer genullt !!
  ClearAllSamples();

  Need_2d_Output := false;
  If PageControl2.PageIndex = 0 Then Begin // Alle Daten aus einer Datei Lesen
    If Not FileExistsUTF8(edit22.text) Then Begin
      showmessage('Error, could not load "' + edit22.text + '".');
      exit;
    End;
    startval := StrToFloat(edit21.text, defFormat);
    deltaval := StrToFloat(edit23.text, defFormat);
    x := startval;
    f := TFileStream.Create(utf8tosys(edit22.text), fmopenread);
    type_0 := 0;
    type_1 := 0;
    type_2 := 0;
    type_3 := 0;
    type_4 := 0;
    type_5 := 0;
    type_6 := 0;
    Case RadioGroup3.ItemIndex Of
      0: Begin // Unsigned 8 Bit
          blocksize := sizeof(type_0);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_0, sizeof(type_0));
            SaveSample(x, type_0);
            x := x + DeltaVal;
          End;
        End;
      1: Begin // Signed 8 Bit
          blocksize := sizeof(type_1);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_1, sizeof(type_1));
            SaveSample(x, type_1);
            x := x + DeltaVal;
          End;
        End;
      2: Begin // Unsigned 16 Bit
          blocksize := sizeof(type_2);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_2, sizeof(type_2));
            SaveSample(x, type_2);
            x := x + DeltaVal;
          End;
        End;
      3: Begin // Signed 16 Bit
          blocksize := sizeof(type_3);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_3, sizeof(type_3));
            SaveSample(x, type_3);
            x := x + DeltaVal;
          End;
        End;
      4: Begin // Unsigned 32 Bit
          blocksize := sizeof(type_4);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_4, sizeof(type_4));
            SaveSample(x, type_4);
            x := x + DeltaVal;
          End;
        End;
      5: Begin // Signed 32 Bit
          blocksize := sizeof(type_5);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_5, sizeof(type_5));
            SaveSample(x, type_5);
            x := x + DeltaVal;
          End;
        End;
      6: Begin // Single
          blocksize := sizeof(type_6);
          For i := 0 To (f.Size Div blocksize) Do Begin
            f.Read(type_6, sizeof(type_6));
            SaveSample(x, type_6);
            x := x + DeltaVal;
          End;
        End;
      7: Begin // nicht benutzt
          showmessage('Error this Option is not avialble.');
          exit;
        End;
      8, 9: Begin // Grayscale Image +  RGB Image
          // Laden des Bildes
          f.free; // Freigeben des Handels, sonst kann TBitmap, die Datei nicht Laden
          f := Nil;
          If lowercase(ExtractFileExt(edit22.text)) = '.bmp' Then Begin
            Daten_D2.LoadFromFile(utf8tosys(edit22.text));
          End
          Else Begin
            If lowercase(ExtractFileExt(edit22.text)) = '.png' Then Begin
              png := TPNGImage.create;
              png.LoadFromFile(utf8tosys(edit22.text));
              Daten_D2.Assign(png);
              png.free;
            End
            Else Begin
              If lowercase(ExtractFileExt(edit22.text)) = '.jpg' Then Begin
                jpg := TjpgImage.create;
                jpg.LoadFromFile(utf8tosys(edit22.text));
                Daten_D2.Assign(jpg);
                jpg.free;
              End;
            End;
          End;
          // Erzeugen der FPImage Daten
          Need_2d_Output := True;
        End;
    End;
    If assigned(f) Then Begin
      f.free;
    End;
  End;
  If PageControl2.PageIndex = 1 Then Begin // Alle Daten sammeln via Formel Input
    // Start der Analyse
    startval := StrToFloat(edit1.text, defFormat);
    Endval := StrToFloat(edit2.text, defFormat);
    deltaval := StrToFloat(edit3.text, defFormat);
    // Erzeugen der Formel
    If Not solver.Create_Formula(memo1.Text) Then Begin
      showmessage('Error in formula, please fix.');
      exit;
    End;
    // ein Bischen Error Handling
    If DeltaVal = 0 Then Begin
      showmessage('Error, delta = 0.0');
      exit;
    End;
    // Auswerten der Formel
    x := startval;
    // Aufsteigende Schleife
    If (EndVal >= StartVal) Then Begin
      If DeltaVal < 0 Then Begin
        showmessage('Error, delta < 0.0');
        exit;
      End;
      While x <= endval Do Begin
        y := solver.EvalFormula(x);
        SaveSample(x, y);
        x := x + DeltaVal;
      End;
    End;
    // Fallende Schleife
    If (EndVal < StartVal) Then Begin
      If DeltaVal > 0 Then Begin
        showmessage('Error, delta > 0.0');
        exit;
      End;
      While x >= endval Do Begin
        y := solver.EvalFormula(x);
        SaveSample(x, y);
        x := x + DeltaVal;
      End;
    End;
  End;
  // Alle "Daten" sind Gesammelt, nun wird verarbeitet!!
  // Auslesen aller Filterdaten
  fir.K := strtoint(edit12.text);
  fir.L := strtoint(edit13.text);
  setlength(fir.C, fir.K + fir.L + 1);
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    fir.C[i - 1] := strtofloat(StringGrid1.Cells[1, i], defFormat);
  End;
  fir.out_of_range := RadioGroup1.ItemIndex;
  fir.out_of_range_value := strtofloat(edit16.text, defFormat);
  fir.cs := strtofloat(edit14.text, defFormat);
  iir.M := strtoint(edit17.text);
  setlength(iir.D, iir.m);
  For i := 1 To StringGrid2.RowCount - 1 Do Begin
    iir.D[i - 1] := strtofloat(StringGrid2.Cells[1, i], defFormat);
  End;
  iir.out_of_range := RadioGroup2.ItemIndex;
  iir.out_of_range_value := strtofloat(edit18.text, defFormat);
  iir.ds := strtofloat(edit19.text, defFormat);
  // Starten der Verarbeitung durch die Filter
  If ((RadioGroup3.ItemIndex >= 0) And (RadioGroup3.ItemIndex <= 6)) Or (PageControl2.PageIndex = 1) Then Begin
    // 1-D Daten werden Bearbeitet
    CalcFilter();
  End
  Else Begin
    // 2-D Daten werden Bearbeitet
    CalcFilter2D();
  End;
  // Alles Verarbeitet, nun wird ausgegeben
  If pagecontrol3.PageIndex = 1 Then Begin
    // Anzeige der Daten
    renderproperties.xmin := strtofloat(edit6.text, defFormat);
    renderproperties.xmax := strtofloat(edit7.text, defFormat);
    renderproperties.ymin := strtofloat(edit4.text, defFormat);
    renderproperties.ymax := strtofloat(edit5.text, defFormat);
    renderproperties.show_orig := CheckBox1.Checked;
    renderproperties.orig_color := image1.Color;
    renderproperties.dest_color := image2.Color;
    renderproperties.show_x_achis := CheckBox2.Checked;
    renderproperties.show_y_achis := CheckBox4.Checked;
    renderproperties.mark_x_achis := CheckBox3.Checked;
    renderproperties.mark_y_achis := CheckBox5.Checked;
    renderproperties.mark_color := image4.Color;
    renderproperties.mark_x_delta := strtofloat(edit8.text, defFormat);
    renderproperties.mark_y_delta := strtofloat(edit9.text, defFormat);
    renderproperties.enhance_mark_x_achis := CheckBox6.Checked;
    renderproperties.enhance_mark_y_achis := CheckBox7.Checked;
    renderproperties.enhance_mark_color := image3.color;
    renderproperties.enhance_mark_x_delta := strtofloat(edit10.text, defFormat);
    renderproperties.enhance_mark_y_delta := strtofloat(edit11.text, defFormat);
    form2.show;
    form2.PaintBox1.Invalidate;
  End
  Else Begin
    // Falls Form2 Sichtbar ist schalten wir sie ab.
    If Form2.Visible Then Form2.Visible := false;
  End;
  If pagecontrol3.PageIndex = 0 Then Begin
    // Speichern Als Datei
    res_f_name := edit26.text;
    If CheckBox12.Checked And FileExistsUTF8(edit26.text) Then Begin
      Form5_result_value := -1;
      form5.Label1.Caption := 'The File :' + LineEnding + res_f_name + LineEnding + 'already exists, what do you want to do?';
      form5.Showmodal;
      If Form5_result_value = -1 Then exit; // Abbruch
      // If Form5_result_value = 0 Then begin end; // Datei Überschreiben
      If Form5_result_value = 1 Then Begin // Neuer Datei namen
        res_f_name := form5.SaveDialog1.FileName;
      End;
    End;
    If Need_2d_Output And (RadioGroup4.itemindex >= 0) And (RadioGroup4.itemindex <= 7) Then Begin
      showmessage('Error, 2D data could not saved as 1D stream.');
      exit;
    End;
    If (Not Need_2d_Output) And ((RadioGroup4.itemindex = 8) Or (RadioGroup4.itemindex = 9)) Then Begin
      showmessage('Error, 1D data could not saved as 2D Image.' + LineEnding + 'Use right mouse button and "save image" in the show output window.');
      exit;
    End;
    Try
      f := TFileStream.Create(utf8tosys(res_f_name), fmcreate Or fmopenwrite);
    Except
      showmessage('Error, unable to create file : ' + LineEnding + res_f_name + LineEnding + 'nothing will be saved.');
      exit;
    End;
    Case RadioGroup4.itemindex Of
      0: Begin // Unsigned 8 Bit
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_0);
            For i := 0 To sample_cnt Do Begin
              type_0 := round(results[i].y);
              f.write(type_0, sizeof(type_0));
            End;
          End;
        End;
      1: Begin // Signed 8 Bit
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_1);
            For i := 0 To sample_cnt Do Begin
              type_1 := round(results[i].y);
              f.write(type_1, sizeof(type_1));
            End;
          End;
        End;
      2: Begin // Unsigned 16 Bit
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_2);
            For i := 0 To sample_cnt Do Begin
              type_2 := round(results[i].y);
              f.write(type_2, sizeof(type_2));
            End;
          End;
        End;
      3: Begin // Signed 16 Bit
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_3);
            For i := 0 To sample_cnt Do Begin
              type_3 := round(results[i].y);
              f.write(type_3, sizeof(type_3));
            End;
          End;
        End;
      4: Begin // Unsigned 32 Bit
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_4);
            For i := 0 To sample_cnt Do Begin
              type_4 := round(results[i].y);
              f.write(type_4, sizeof(type_4));
            End;
          End;
        End;
      5: Begin // Signed 32 Bit
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_5);
            For i := 0 To sample_cnt Do Begin
              type_5 := round(results[i].y);
              f.write(type_5, sizeof(type_5));
            End;
          End;
        End;
      6: Begin // Single
          If assigned(results) And (sample_cnt > 0) Then Begin
            blocksize := sizeof(type_6);
            For i := 0 To sample_cnt Do Begin
              type_6 := results[i].y;
              f.write(type_6, sizeof(type_6));
            End;
          End;
        End;
      7: Begin // nicht benutzt
          showmessage('Error this Option is not avialble.');
          exit;
        End;
      8, 9: Begin // Grayscale Image +  RGB Image Die Unterscheidung wird während des Filterns getroffen
          // Speichern des Bildes
          f.free;
          f := Nil;
          If lowercase(ExtractFileExt(res_f_name)) = '.bmp' Then Begin
            Daten_D2.SaveToFile(utf8tosys(res_f_name));
          End
          Else Begin
            If lowercase(ExtractFileExt(res_f_name)) = '.png' Then Begin
              png := TPNGImage.create;
              png.Assign(Daten_D2);
              png.SaveToFile(utf8tosys(res_f_name));
              png.free;
            End
            Else Begin
              If lowercase(ExtractFileExt(res_f_name)) = '.jpg' Then Begin
                jpg := TjpgImage.create;
                jpg.Assign(Daten_D2);
                jpg.SaveToFile(utf8tosys(res_f_name));
                jpg.free;
              End;
            End;
          End;
        End;
    End;
    If assigned(f) Then
      f.free;
  End;
End;

Procedure TForm1.Button20Click(Sender: TObject);
Begin
  form4.show;
End;

Procedure TForm1.Button21Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.Cells[1, i] := '';
  End;
  edit14.text := '0.0';
End;

Procedure TForm1.Button22Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 1 To StringGrid2.RowCount - 1 Do Begin
    StringGrid2.Cells[1, i] := '';
  End;
  edit19.text := '0.0';
End;

Procedure TForm1.Button23Click(Sender: TObject);
Begin
  If ((RadioGroup4.ItemIndex >= 0) And (RadioGroup4.ItemIndex <= 6)) Then Begin
    saveDialog4.FilterIndex := 1;
    //saveDialog4.DefaultExt := '.bin';
  End
  Else Begin
    saveDialog4.FilterIndex := 2;
    //saveDialog4.DefaultExt := '.bmp';
  End;
  If SaveDialog4.Execute Then Begin
    SaveDialog4.InitialDir := ExtractFilePath(SaveDialog4.FileName);
    OpenDialog4.InitialDir := ExtractFilePath(SaveDialog4.FileName);
    edit26.text := SaveDialog4.FileName;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  i: TIniFile;
Begin
  If openDialog2.Execute Then Begin
    SaveDialog2.InitialDir := ExtractFilePath(openDialog2.FileName);
    OpenDialog2.InitialDir := ExtractFilePath(openDialog2.FileName);
    label21.caption := ExtractFileNameOnly(openDialog2.FileName);
    i := TIniFile.Create(utf8tosys(openDialog2.FileName));
    Edit1.text := i.ReadString('Input Data', 'FormulaStart', '');
    Edit2.text := i.ReadString('Input Data', 'FormulaEnd', '');
    Edit3.text := i.ReadString('Input Data', 'FormulaDelta', '');
    Memo1.lines.DelimitedText := i.ReadString('Input Data', 'Formula', '');
    i.free;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  i: TIniFile;
Begin
  If SaveDialog2.Execute Then Begin
    SaveDialog2.InitialDir := ExtractFilePath(SaveDialog2.FileName);
    OpenDialog2.InitialDir := ExtractFilePath(SaveDialog2.FileName);
    label21.caption := ExtractFileNameOnly(SaveDialog2.FileName);
    i := TIniFile.Create(utf8tosys(SaveDialog2.FileName));
    i.WriteString('Input Data', 'FormulaStart', Edit1.text);
    i.WriteString('Input Data', 'FormulaEnd', Edit2.text);
    i.WriteString('Input Data', 'FormulaDelta', Edit3.text);
    i.WriteString('Input Data', 'Formula', '"' + Memo1.lines.DelimitedText + '"');
    i.free;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  If (RadioGroup3.ItemIndex >= 0) And (RadioGroup3.ItemIndex <= 6) Then Begin
    OpenDialog4.FilterIndex := 1;
    //OpenDialog4.DefaultExt := '.bin';
  End
  Else Begin
    OpenDialog4.FilterIndex := 2;
    //OpenDialog4.DefaultExt := '.bmp';
  End;
  If OpenDialog4.Execute Then Begin
    SaveDialog4.InitialDir := ExtractFilePath(OpenDialog4.FileName);
    OpenDialog4.InitialDir := ExtractFilePath(OpenDialog4.FileName);
    edit22.text := OpenDialog4.FileName;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  memo1.Clear;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  i: TIniFile;
Begin
  If SaveDialog3.Execute Then Begin
    SaveDialog3.InitialDir := ExtractFilePath(SaveDialog3.FileName);
    OpenDialog3.InitialDir := ExtractFilePath(SaveDialog3.FileName);
    label22.caption := ExtractFileNameOnly(SaveDialog3.FileName);
    i := TIniFile.Create(utf8tosys(SaveDialog3.FileName));
    i.WriteString('Output Data', 'x-min', edit6.text);
    i.WriteString('Output Data', 'y-min', edit4.text);
    i.WriteString('Output Data', 'x-max', edit7.text);
    i.WriteString('Output Data', 'y-max', edit5.text);
    i.WriteBool('Output Data', 'source', CheckBox1.Checked);
    i.WriteString('Output Data', 'sourcecolor', ColorToString(image1.Color));
    i.WriteString('Output Data', 'destcolor', ColorToString(image2.Color));
    i.WriteBool('Output Data', 'showx', CheckBox2.Checked);
    i.WriteBool('Output Data', 'showy', CheckBox4.Checked);
    i.WriteBool('Output Data', 'markx', CheckBox3.Checked);
    i.WriteBool('Output Data', 'marky', CheckBox5.Checked);
    i.WriteString('Output Data', 'x-delta', edit8.text);
    i.WriteString('Output Data', 'y-delta', edit9.text);
    i.WriteString('Output Data', 'markcolor', ColorToString(image4.Color));
    i.WriteBool('Output Data', 'enhancex', CheckBox6.Checked);
    i.WriteBool('Output Data', 'enhancey', CheckBox7.Checked);
    i.WriteString('Output Data', 'enhance-x-delta', edit10.text);
    i.WriteString('Output Data', 'enhance-y-delta', edit11.text);
    i.WriteString('Output Data', 'enhancecolor', ColorToString(image3.Color));
    i.free;
  End;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  i: TIniFile;
Begin
  If OpenDialog3.Execute Then Begin
    SaveDialog3.InitialDir := ExtractFilePath(openDialog3.FileName);
    OpenDialog3.InitialDir := ExtractFilePath(openDialog3.FileName);
    label22.caption := ExtractFileNameOnly(openDialog3.FileName);
    i := TIniFile.Create(UTF8ToSys(openDialog3.FileName));
    edit6.text := i.ReadString('Output Data', 'x-min', edit6.text);
    edit4.text := i.ReadString('Output Data', 'y-min', edit4.text);
    edit7.text := i.ReadString('Output Data', 'x-max', edit7.text);
    edit5.text := i.ReadString('Output Data', 'y-max', edit5.text);
    CheckBox1.Checked := i.ReadBool('Output Data', 'source', CheckBox1.Checked);
    image1.Color := StringToColor(i.ReadString('Output Data', 'sourcecolor', ColorToString(image1.Color)));
    image2.Color := StringToColor(i.ReadString('Output Data', 'destcolor', ColorToString(image2.Color)));
    CheckBox2.Checked := i.ReadBool('Output Data', 'showx', CheckBox2.Checked);
    CheckBox4.Checked := i.ReadBool('Output Data', 'showy', CheckBox4.Checked);
    CheckBox3.Checked := i.ReadBool('Output Data', 'markx', CheckBox3.Checked);
    CheckBox5.Checked := i.ReadBool('Output Data', 'marky', CheckBox5.Checked);
    edit8.text := i.ReadString('Output Data', 'x-delta', edit8.text);
    edit9.text := i.ReadString('Output Data', 'y-delta', edit9.text);
    image4.Color := StringToColor(i.ReadString('Output Data', 'markcolor', ColorToString(image4.Color)));
    CheckBox6.Checked := i.ReadBool('Output Data', 'enhancex', CheckBox6.Checked);
    CheckBox7.Checked := i.ReadBool('Output Data', 'enhancey', CheckBox7.Checked);
    edit10.text := i.ReadString('Output Data', 'enhance-x-delta', edit10.text);
    edit11.text := i.ReadString('Output Data', 'enhance-y-delta', edit11.text);
    image3.Color := StringToColor(i.ReadString('Output Data', 'enhancecolor', ColorToString(image3.Color)));
    i.free;
    PageControl1Change(Nil);
  End;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  ColorDialog1.Color := Image1.Color;
  If ColorDialog1.Execute Then Begin
    Image1.Color := ColorDialog1.Color;
    PageControl1Change(Nil);
  End;
End;

Procedure TForm1.Edit3KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = ',' Then key := '.';
  If key = #13 Then Begin
    If (Sender = Edit12) Or (Sender = Edit12) Then Button17.OnClick(Nil);
    If (sender = Edit16) Then Button16.OnClick(Nil);
    If (sender = Edit17) Then Button18.OnClick(Nil);
    If (sender = Edit20) Then Button19.OnClick(Nil);
  End;
End;

Procedure TForm1.Button10Click(Sender: TObject);
Begin
  ColorDialog1.Color := Image2.Color;
  If ColorDialog1.Execute Then Begin
    Image2.Color := ColorDialog1.Color;
    PageControl1Change(Nil);
  End;
End;

Procedure TForm1.Button11Click(Sender: TObject);
Begin
  ColorDialog1.Color := Image3.Color;
  If ColorDialog1.Execute Then Begin
    Image3.Color := ColorDialog1.Color;
    PageControl1Change(Nil);
  End;
End;

Procedure TForm1.Button12Click(Sender: TObject);
Begin
  ColorDialog1.Color := Image4.Color;
  If ColorDialog1.Execute Then Begin
    Image4.Color := ColorDialog1.Color;
    PageControl1Change(Nil);
  End;
End;

Procedure TForm1.Button13Click(Sender: TObject);
Var
  ini: TIniFile;
  i: integer;
  dummy: word;
Begin
  If OpenDialog1.Execute Then Begin
    SaveDialog1.InitialDir := ExtractFilePath(openDialog1.FileName);
    OpenDialog1.InitialDir := ExtractFilePath(openDialog1.FileName);
    label20.caption := ExtractFileNameOnly(openDialog1.FileName);
    ini := TIniFile.Create(utf8tosys(OpenDialog1.FileName));
    edit12.text := ini.ReadString('FIR Options', 'k', edit12.text);
    edit13.text := ini.ReadString('FIR Options', 'l', edit13.text);
    RadioGroup1.ItemIndex := ini.Readinteger('FIR Options', 'Out of range', RadioGroup1.ItemIndex);
    edit16.text := ini.ReadString('FIR Options', 'Out of range value', edit16.text);
    edit14.text := ini.ReadString('FIR Options', 'cs', edit14.text);
    button17.OnClick(Nil);
    StringGrid1.RowCount := ini.Readinteger('FIR Options', 'n', StringGrid1.RowCount);
    For i := 1 To StringGrid1.RowCount - 1 Do Begin
      StringGrid1.cells[1, i] := ini.ReadString('FIR Options', 'index' + inttostr(i), StringGrid1.cells[1, i]);
    End;
    dummy := 13;
    StringGrid1KeyUp(Nil, dummy, []); // Aktualisieren der summe cn
    edit17.text := ini.ReadString('IIR Options', 'm', edit17.text);
    RadioGroup2.ItemIndex := ini.Readinteger('IIR Options', 'Out of range', RadioGroup2.ItemIndex);
    edit18.text := ini.ReadString('IIR Options', 'Out of range value', edit18.text);
    edit19.text := ini.ReadString('IIR Options', 'ds', edit19.text);
    button18.OnClick(Nil);
    StringGrid2.RowCount := ini.Readinteger('IIR Options', 'n', StringGrid2.RowCount);
    For i := 1 To StringGrid2.RowCount - 1 Do Begin
      StringGrid2.cells[1, i] := ini.ReadString('IIR Options', 'index' + inttostr(i), StringGrid2.cells[1, i]);
    End;
    StringGrid2KeyUp(Nil, dummy, []); // Aktualisieren der summe dn
    ini.free;
  End;
End;

Procedure TForm1.Button14Click(Sender: TObject);
Var
  ini: TIniFile;
  i: integer;
Begin
  If SaveDialog1.Execute Then Begin
    SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
    OpenDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
    label20.caption := ExtractFileNameOnly(SaveDialog1.FileName);
    ini := TIniFile.Create(utf8tosys(SaveDialog1.FileName));
    ini.WriteString('FIR Options', 'k', edit12.text);
    ini.WriteString('FIR Options', 'l', edit13.text);
    ini.Writeinteger('FIR Options', 'Out of range', RadioGroup1.ItemIndex);
    ini.WriteString('FIR Options', 'Out of range value', edit16.text);
    ini.WriteString('FIR Options', 'cs', edit14.text);
    ini.Writeinteger('FIR Options', 'n', StringGrid1.RowCount);
    For i := 1 To StringGrid1.RowCount - 1 Do Begin
      ini.WriteString('FIR Options', 'index' + inttostr(i), StringGrid1.cells[1, i]);
    End;
    ini.WriteString('IIR Options', 'm', edit17.text);
    ini.Writeinteger('IIR Options', 'Out of range', RadioGroup2.ItemIndex);
    ini.WriteString('IIR Options', 'Out of range value', edit18.text);
    ini.WriteString('IIR Options', 'ds', edit19.text);
    ini.Writeinteger('IIR Options', 'n', StringGrid2.RowCount);
    For i := 1 To StringGrid2.RowCount - 1 Do Begin
      ini.WriteString('IIR Options', 'index' + inttostr(i), StringGrid2.cells[1, i]);
    End;
    ini.free;
  End;
End;

Procedure TForm1.Button15Click(Sender: TObject);
Begin
  form3.show;
End;

Procedure TForm1.Button16Click(Sender: TObject);
Var
  d: TDatatype;
Begin
  d := StrToFloat(edit15.text, defFormat);
  If d <> 0 Then Begin
    edit14.text := floattostr(1 / d, defFormat);
  End
  Else
    edit14.text := '0.0';
End;

Procedure TForm1.Button17Click(Sender: TObject);
Var
  i, k, l: integer;
Begin
  k := strtoint(edit12.text);
  l := strtoint(edit13.text);
  StringGrid1.RowCount := k + l + 2;
  StringGrid1.Cells[0, 0] := 'Index';
  StringGrid1.Cells[1, 0] := 'Value';
  For i := 1 To k + l + 1 Do Begin
    StringGrid1.Cells[0, i] := 'C' + inttostr(i - k - 1);
  End;
End;

Procedure TForm1.Button18Click(Sender: TObject);
Var
  i, m: integer;
Begin
  m := strtoint(edit17.text);
  StringGrid2.RowCount := m + 1;
  StringGrid2.Cells[0, 0] := 'Index';
  StringGrid2.Cells[1, 0] := 'Value';
  For i := 1 To StringGrid2.RowCount - 1 Do Begin
    StringGrid2.Cells[0, i] := 'd' + inttostr(i - m - 1);
  End;
End;

Procedure TForm1.Button19Click(Sender: TObject);
Var
  d: TDatatype;
Begin
  d := StrToFloat(edit20.text, defFormat);
  If d <> 0 Then Begin
    edit19.text := floattostr(1 / d, defFormat);
  End
  Else
    edit19.text := '0.0';
End;

End.

