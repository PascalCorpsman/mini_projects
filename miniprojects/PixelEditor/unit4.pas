(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
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

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Shape1: TShape;
    Procedure FormCreate(Sender: TObject);
    Procedure Shape1Click(Sender: TObject);
  private

  public

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

{ TForm4 }

Procedure TForm4.Shape1Click(Sender: TObject);
Begin
  ColorDialog1.Color := Shape1.Brush.Color;
  If ColorDialog1.Execute Then Begin
    Shape1.Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  // If you get a compiler error here, your Lazarus version is "too old"
  // Just uncomment this line and you can use the app without the transparent color select feature for .bmp graphics
  Shape1.OnClick := @Shape1Click;
End;

End.

