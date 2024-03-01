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
Unit Unit2;

{$MODE objfpc}{$H+}

Interface

Uses
  dglopengl, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, OpenGLContext, uparken;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    ScrollBar5: TScrollBar;
    ScrollBar6: TScrollBar;
    ScrollBar7: TScrollBar;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure ScrollBar3Change(Sender: TObject);
    Procedure ScrollBar4Change(Sender: TObject);
    Procedure ScrollBar5Change(Sender: TObject);
    Procedure ScrollBar6Change(Sender: TObject);
    Procedure ScrollBar7Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses unit1, lazutf8;

{ TForm2 }

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    EditCar.LoadFromFile(systoutf8(OpenDialog1.FileName));
  End;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    EditCar.SaveToFile(systoutf8(SaveDialog1.FileName));
  End;
End;

Procedure TForm2.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  GameState := gswait;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Car properties';
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  saveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  // Größe des Fensters Festlegen
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm2.ScrollBar1Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.Alpha := ScrollBar1.Position;
  EditCar.Angle := EditCar.Alpha;
End;

Procedure TForm2.ScrollBar2Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.H1 := TScrollbar(sender).Position;
End;

Procedure TForm2.ScrollBar3Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.H2 := TScrollbar(sender).Position;
End;

Procedure TForm2.ScrollBar4Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.H3 := TScrollbar(sender).Position;
End;

Procedure TForm2.ScrollBar5Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.w1 := TScrollbar(sender).Position;
End;

Procedure TForm2.ScrollBar6Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.w2 := TScrollbar(sender).Position;
End;

Procedure TForm2.ScrollBar7Change(Sender: TObject);
Begin
  TScrollbar(sender).Hint := inttostr(TScrollbar(sender).Position);
  EditCar.H4 := TScrollbar(sender).Position;
End;

End.

