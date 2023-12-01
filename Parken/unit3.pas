(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Parken                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uparken;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    ScrollBar5: TScrollBar;
    ScrollBar6: TScrollBar;
    ScrollBar7: TScrollBar;
    ScrollBar8: TScrollBar;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure ScrollBar3Change(Sender: TObject);
    Procedure ScrollBar4Change(Sender: TObject);
    Procedure ScrollBar5Change(Sender: TObject);
    Procedure ScrollBar6Change(Sender: TObject);
    Procedure ScrollBar7Change(Sender: TObject);
    Procedure ScrollBar8Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses unit1, lazutf8;

{ TForm3 }

Procedure TForm3.ComboBox1Change(Sender: TObject);
Begin
  Case ComboBox1.ItemIndex Of
    0: Begin
        ScrollBar2.Visible := true;
        ScrollBar3.Visible := true;
        ScrollBar4.Visible := true;
        ScrollBar7.Visible := true;
        ScrollBar5.Visible := true;
        ScrollBar6.Visible := true;
        ScrollBar8.Visible := false;
        Label2.Visible := true;
        Label3.Visible := true;
        Label4.Visible := true;
        Label7.Visible := true;
        Label5.Visible := true;
        Label6.Visible := true;
        Label8.Visible := false;
        Edittrailer := EditEinAchsenAnhaenger;
        ScrollBar2.Position := round(EditEinAchsenAnhaenger.h1);
        ScrollBar3.Position := round(EditEinAchsenAnhaenger.h2);
        ScrollBar4.Position := round(EditEinAchsenAnhaenger.h3);
        ScrollBar7.Position := round(EditEinAchsenAnhaenger.h4);
        ScrollBar5.Position := round(EditEinAchsenAnhaenger.w1);
        ScrollBar6.Position := round(EditEinAchsenAnhaenger.w2);
      End;
    1: Begin
        ScrollBar2.Visible := true;
        ScrollBar3.Visible := true;
        ScrollBar4.Visible := true;
        ScrollBar7.Visible := true;
        ScrollBar5.Visible := true;
        ScrollBar6.Visible := true;
        ScrollBar8.Visible := true;
        Label2.Visible := true;
        Label3.Visible := true;
        Label4.Visible := true;
        Label7.Visible := true;
        Label5.Visible := true;
        Label6.Visible := true;
        Label8.Visible := True;
        Edittrailer := EditZweiAchsenAnhaenger;
        ScrollBar2.Position := round(EditZweiAchsenAnhaenger.h1);
        ScrollBar3.Position := round(EditZweiAchsenAnhaenger.h2);
        ScrollBar4.Position := round(EditZweiAchsenAnhaenger.h3);
        ScrollBar7.Position := round(EditZweiAchsenAnhaenger.h4);
        ScrollBar8.Position := round(EditZweiAchsenAnhaenger.h5);
        ScrollBar5.Position := round(EditZweiAchsenAnhaenger.w1);
        ScrollBar6.Position := round(EditZweiAchsenAnhaenger.w2);
      End;
  End;

End;

Procedure TForm3.Button3Click(Sender: TObject);
Var
  f: TFilestream;
  i: Integer;
Begin
  If OpenDialog1.Execute Then Begin
    f := TFileStream.create(systoutf8(OpenDialog1.FileName), fmOpenRead);
    i := -1;
    f.read(i, sizeof(i));
    Case i Of
      0: EditEinAchsenAnhaenger.LoadFromStream(f);
      1: EditZweiAchsenAnhaenger.LoadFromStream(f);
    End;
    ComboBox1.ItemIndex := i;
    ComboBox1Change(Nil);
    f.free;
  End;
End;

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.Button4Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    Edittrailer.SaveToFile(systoutf8(SaveDialog1.FileName));
  End;
End;

Procedure TForm3.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  GameState := gswait;
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Trailer properties';
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  saveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  // Größe des Fensters Festlegen
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm3.ScrollBar2Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    0: EditEinAchsenAnhaenger.H1 := TScrollbar(sender).Position;
    1: EditZweiAchsenAnhaenger.H1 := TScrollbar(sender).Position;
  End;
End;

Procedure TForm3.ScrollBar3Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    0: EditEinAchsenAnhaenger.H2 := TScrollbar(sender).Position;
    1: EditZweiAchsenAnhaenger.H2 := TScrollbar(sender).Position;
  End;
End;

Procedure TForm3.ScrollBar4Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    0: EditEinAchsenAnhaenger.H3 := TScrollbar(sender).Position;
    1: EditZweiAchsenAnhaenger.H3 := TScrollbar(sender).Position;
  End;
End;

Procedure TForm3.ScrollBar5Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    0: EditEinAchsenAnhaenger.w1 := TScrollbar(sender).Position;
    1: EditZweiAchsenAnhaenger.w1 := TScrollbar(sender).Position;
  End;
End;

Procedure TForm3.ScrollBar6Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    0: EditEinAchsenAnhaenger.w2 := TScrollbar(sender).Position;
    1: EditZweiAchsenAnhaenger.w2 := TScrollbar(sender).Position;
  End;
End;

Procedure TForm3.ScrollBar7Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    0: EditEinAchsenAnhaenger.h4 := TScrollbar(sender).Position;
    1: EditZweiAchsenAnhaenger.h4 := TScrollbar(sender).Position;
  End;
End;

Procedure TForm3.ScrollBar8Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  Case ComboBox1.ItemIndex Of
    //    0: EditEinAchsenAnhaenger.h5 := TScrollbar(sender).Position; -- Gibt es nicht
    1: EditZweiAchsenAnhaenger.h5 := TScrollbar(sender).Position;
  End;
End;

End.

