(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of KLab                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit8;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, SynHighlighterCpp,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    SynCppSyn1: TSynCppSyn;
    SynEdit1: TSynEdit;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
    Procedure SetHG(Color_: TColor);
    Procedure SetVG(Color_: TColor);
    Procedure SetStyle(value: TFontStyles);
    Function getStyle: TFontStyles;
  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

Uses unit1, uklab;

{ TForm8 }

Procedure TForm8.SetHG(Color_: TColor);
Var
  i: Integer;
  P: TPanel;
Begin
  label2.visible := false;
  For i := 1 To 16 Do Begin
    p := TPanel(FindComponent('Panel' + inttostr(i)));
    If p.Color = Color_ Then Begin
      label2.visible := True;
      label2.parent := P;
      label2.left := 2;
      label2.top := 5;
      label3.Font.color := ($00AAAAAA Xor Color_) And $00FFFFFF;
      Exit;
    End;
  End;
End;

Procedure TForm8.SetVG(Color_: TColor);
Var
  i: Integer;
  P: TPanel;
Begin
  label3.visible := false;
  For i := 1 To 16 Do Begin
    p := TPanel(FindComponent('Panel' + inttostr(i)));
    If p.Color = Color_ Then Begin
      label3.visible := true;
      label3.parent := P;
      label3.left := 2;
      label3.top := 5;
      label3.Font.color := ($00AAAAAA Xor Color_) And $00FFFFFF;
      color_ := label3.Font.color;
      Exit;
    End;
  End;
End;

Function TForm8.getStyle: TFontStyles;
Begin
  result := [];
  If checkbox1.checked Then
    result := result + [fsbold];
  If checkbox2.checked Then
    result := result + [fsitalic];
  If checkbox3.checked Then
    result := result + [fsUnderline];
End;

Procedure TForm8.SetStyle(value: TFontStyles);
Begin
  checkbox1.checked := fsbold In Value;
  checkbox2.checked := fsitalic In Value;
  checkbox3.checked := fsUnderline In Value;
End;

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'Sheme Settings';
  Listbox1.itemindex := 0;
  SynEdit1.SelStart := 1;
  SynEdit1.SelEnd := 21;
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm8.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm8.Button3Click(Sender: TObject);
Begin
  form8.SynCppSyn1.AsmAttri.Foreground := defAsmAttri;
  form8.SynCppSyn1.CommentAttri.Foreground := defCommentAttri;
  form8.SynCppSyn1.DirecAttri.Foreground := defDirecAttri;
  form8.SynCppSyn1.IdentifierAttri.Foreground := defIdentifierAttri;
  form8.SynCppSyn1.InvalidAttri.Foreground := defInvalidAttri;
  form8.SynCppSyn1.KeyAttri.Foreground := defKeyAttri;
  form8.SynCppSyn1.NumberAttri.Foreground := defNumberAttri;
  form8.SynEdit1.RightEdgeColor := defRightEdge;
  form8.SynEdit1.Color := defSpaceAttri;
  form8.SynCppSyn1.StringAttri.Foreground := defStringAttri;
  form8.SynCppSyn1.SymbolAttri.Foreground := defSymbolAttri;
  form8.SynEdit1.SelectedColor.Background := defSelectedColor;
  form8.SynCppSyn1.AsmAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.CommentAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.DirecAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.IdentifierAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.InvalidAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.KeyAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.NumberAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.StringAttri.Background := defBackgroundColor;
  form8.SynCppSyn1.SymbolAttri.Background := defBackgroundColor;
  form8.SynEdit1.SelectedColor.Foreground := defSelectedForegroundColor;
  // Todo : Set Default Sheme Settings Style
End;

Procedure TForm8.CheckBox1Click(Sender: TObject);
Var
  f: TFontStyles;
Begin
  f := getStyle;
  Case Listbox1.itemindex Of
    // AsmAttri
    0: SynCppSyn1.AsmAttri.style := f;
    // CommentAttri
    1: SynCppSyn1.CommentAttri.style := f;
    // DirectiveAttri
    2: SynCppSyn1.DirecAttri.style := f;
    // IdentifierAttri
    3: SynCppSyn1.IdentifierAttri.style := f;
    // InvalidAttri
    4: SynCppSyn1.InvalidAttri.style := f;
    // KeyAttri
    5: SynCppSyn1.KeyAttri.style := f;
    // NumberAttri
    6: SynCppSyn1.NumberAttri.style := f;
    // RightEdgeColor
    7: Begin
      End;
    // SpaceAttri
    8: SynCppSyn1.SpaceAttri.style := f;
    // StringAttri
    9: SynCppSyn1.StringAttri.style := f;
    // SymbolAttri
    10: SynCppSyn1.SymbolAttri.style := f;
    // Synedit1.SelectedColor
    11: synedit1.SelectedColor.Style := f;
  End;
End;

Procedure TForm8.Button1Click(Sender: TObject);
Begin
  // Speichern der Aktuellen Settings nach Form1
  form1.Set_Highlighter;
  close;
End;

Procedure TForm8.ListBox1Click(Sender: TObject);
Begin
  Case Listbox1.itemindex Of
    // AsmAttri
    0: Begin
        SetVG(SynCppSyn1.AsmAttri.Foreground);
        SetHG(SynCppSyn1.AsmAttri.Background);
        SetStyle(SynCppSyn1.AsmAttri.Style);
      End;
    // CommentAttri
    1: Begin
        SetVG(SynCppSyn1.CommentAttri.Foreground);
        SetHG(SynCppSyn1.CommentAttri.Background);
        SetStyle(SynCppSyn1.CommentAttri.Style);
      End;
    // DirecAttri
    2: Begin
        SetVG(SynCppSyn1.DirecAttri.Foreground);
        SetHG(SynCppSyn1.DirecAttri.Background);
        SetStyle(SynCppSyn1.DirecAttri.Style);
      End;
    // IdentifierAttri
    3: Begin
        SetVG(SynCppSyn1.IdentifierAttri.Foreground);
        SetHG(SynCppSyn1.IdentifierAttri.Background);
        SetStyle(SynCppSyn1.IdentifierAttri.Style);
      End;
    // InvalidAttri
    4: Begin
        SetVG(SynCppSyn1.InvalidAttri.Foreground);
        SetHG(SynCppSyn1.InvalidAttri.Background);
        SetStyle(SynCppSyn1.InvalidAttri.Style);
      End;
    // KeyAttri
    5: Begin
        SetVG(SynCppSyn1.KeyAttri.Foreground);
        SetHG(SynCppSyn1.KeyAttri.Background);
        SetStyle(SynCppSyn1.KeyAttri.Style);
      End;
    // NumberAttri
    6: Begin
        SetVG(SynCppSyn1.NumberAttri.Foreground);
        SetHG(SynCppSyn1.NumberAttri.Background);
        SetStyle(SynCppSyn1.NumberAttri.Style);
      End;
    //  RightEdge
    7: Begin
        SetVG(synedit1.RightEdgeColor);
        SetHG(clnone);
        setstyle([]);
      End;
    // SpaceAttri
    8: Begin
        SetVG(clnone);
        SetHG(SynEdit1.Color);
        SetStyle([]);
      End;
    // StringAttri
    9: Begin
        SetVG(SynCppSyn1.StringAttri.Foreground);
        SetHG(SynCppSyn1.StringAttri.Background);
        SetStyle(SynCppSyn1.StringAttri.Style);
      End;
    // SymbolAttri
    10: Begin
        SetVG(SynCppSyn1.SymbolAttri.Foreground);
        SetHG(SynCppSyn1.SymbolAttri.Background);
        SetStyle(SynCppSyn1.SymbolAttri.Style);
      End;
    // Synedit1.SelectedColor
    11: Begin
        SetVG(Synedit1.SelectedColor.Foreground);
        SetHG(Synedit1.SelectedColor.Background);
        SetStyle(Synedit1.SelectedColor.Style);
      End;
  End;
End;

Procedure TForm8.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    Case Listbox1.itemindex Of
      // AsmAttri
      0: SynCppSyn1.AsmAttri.Foreground := TPanel(sender).color;
      // CommentAttri
      1: SynCppSyn1.CommentAttri.Foreground := TPanel(sender).color;
      // DirectiveAttri
      2: SynCppSyn1.DirecAttri.Foreground := TPanel(sender).color;
      // IdentifierAttri
      3: SynCppSyn1.IdentifierAttri.Foreground := TPanel(sender).color;
      // InvalidAttri
      4: SynCppSyn1.InvalidAttri.Foreground := TPanel(sender).color;
      // KeyAttri
      5: SynCppSyn1.KeyAttri.Foreground := TPanel(sender).color;
      // NumberAttri
      6: SynCppSyn1.NumberAttri.Foreground := TPanel(sender).color;
      // RightEdgeColor
      7: synedit1.RightEdgeColor := TPanel(sender).color;
      // SpaceAttri
      8: Begin
          //synedit1.Color := TPanel(sender).color;
        End;
      // StringAttri
      9: SynCppSyn1.StringAttri.Foreground := TPanel(sender).color;
      // SymbolAttri
      10: SynCppSyn1.SymbolAttri.Foreground := TPanel(sender).color;
      // Synedit1.SelectedColor
      11: Begin
          SynEdit1.SelectedColor.Foreground := TPanel(sender).color;
          SynEdit1.SelStart := 1;
          SynEdit1.SelEnd := 18;
        End;
    End;
    SetVG(TPanel(sender).color);
  End;
  If ssRight In shift Then Begin
    Case Listbox1.itemindex Of
      // AsmAttri
      0: SynCppSyn1.AsmAttri.Background := TPanel(sender).color;
      // CommentAttri
      1: SynCppSyn1.CommentAttri.Background := TPanel(sender).color;
      // DirectiveAttri
      2: SynCppSyn1.DirecAttri.Background := TPanel(sender).color;
      // IdentifierAttri
      3: SynCppSyn1.IdentifierAttri.Background := TPanel(sender).color;
      // Invalid Attri
      4: SynCppSyn1.InvalidAttri.Background := TPanel(sender).color;
      // KeyAttri
      5: SynCppSyn1.KeyAttri.Background := TPanel(sender).color;
      // NumberAttri
      6: SynCppSyn1.NumberAttri.Background := TPanel(sender).color;
      // RightEdgeColor
      7: Synedit1.RightEdgeColor := TPanel(sender).color;
      // SpaceAttri
      8: Begin
          SynCppSyn1.SpaceAttri.Background := TPanel(sender).color;
          SynEdit1.Color := TPanel(sender).color;
        End;
      // StringAttri
      9: SynCppSyn1.StringAttri.Background := TPanel(sender).color;
      // SymbolAttri
      10: SynCppSyn1.SymbolAttri.Background := TPanel(sender).color;
      // Synedit1.SelectedColor
      11: Begin
          SynEdit1.SelectedColor.Background := TPanel(sender).color;
          SynEdit1.SelStart := 1;
          SynEdit1.SelEnd := 18;
        End;
    End;
    SetHG(TPanel(sender).color);
  End;
End;

End.

