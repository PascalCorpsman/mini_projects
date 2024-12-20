(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Sudoku                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit10;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Printers, LResources;

Type
  TForm10 = Class(TForm)
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    ScrollBar1: TScrollBar;
    Label4: TLabel;
    Procedure Button2Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form10: TForm10;
  Drucken: Boolean;

Procedure DruckeWerbung(Printer: TPrinter);

Implementation

Uses Unit1;

{$R *.lfm}

Procedure DruckeWerbung(Printer: TPrinter);
Var
  Breite: Integer;
  Textsize: Integer;
  werbung: String;
Begin
  werbung := 'Created with Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |  Support : http://www.corpsman.de.vu/';
  Breite := Printer.PageWidth Div 33;
  Textsize := 1;
  Printer.canvas.Font.Size := Textsize;
  While Printer.canvas.TextHeight('8') < Breite - (Breite Div 4) Do Begin
    inc(Textsize);
    Printer.canvas.Font.Size := Textsize;
  End;
  Printer.canvas.font.Color := clblack;
  Printer.canvas.font.size := Textsize;
  Printer.canvas.textout((Printer.PageWidth - Printer.canvas.TextWidth(werbung)) Div 2, Printer.PageHeight - breite, werbung);
End;

Procedure TForm10.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm10.Button1Click(Sender: TObject);
Begin
  Druckbreite := scrollbar1.Position;
  drucken := true;
  close;
End;

Procedure TForm10.Edit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (key In ['0'..'9', #8]) Then key := #0;
End;

End.

