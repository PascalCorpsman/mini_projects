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
Unit Unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uparken;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form4: TForm4;
  m, m1: TPoint;

Implementation

{$R *.lfm}

Uses unit1, lazutf8;

{ TForm4 }

Procedure TForm4.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  gamestate := gswait;
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  ColorDialog1.Color := button1.font.color;
  If ColorDialog1.execute Then Begin
    button1.font.color := ColorDialog1.Color;
  End;
End;

Procedure TForm4.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.Button3Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    EditArea.LoadFromFile(systoutf8(OpenDialog1.FileName));
  End;
End;

Procedure TForm4.Button4Click(Sender: TObject);
Begin
  If SaveDialog1.execute Then Begin
    editarea.SafeToFile(systoutf8(SaveDialog1.FileName));
  End;
End;

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Map properties';
  OpenDialog1.InitialDir := ExtractFilePath(paramstrutf8(0));
  saveDialog1.InitialDir := ExtractFilePath(paramstrutf8(0));
  edit1.text := '50';
  // Größe des Fensters Festlegen
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

End.

