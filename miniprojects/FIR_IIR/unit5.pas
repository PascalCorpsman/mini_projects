(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FIR IIR                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, math;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image1: TImage;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form5: TForm5;
  (*
   * -1 = Cancel, Abbruch
   *  0 = Override
   *  1 = Eine Datei selektiert
   *)
  Form5_result_value: Integer;

Implementation

{$R *.lfm}

Var
  f5w: integer;

  { TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  SaveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  f5W := Form5.Width;
  caption := 'Question';
End;

Procedure TForm5.FormShow(Sender: TObject);
Var
  w: integer;
Begin
  // Anpassen der Breite
  W := max(f5W, form5.Label1.Width + 10 + form5.Label1.Left);
  Tform(self).Constraints.Maxwidth := w;
  Tform(self).Constraints.Minwidth := w;
End;

Procedure TForm5.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm5.Button3Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    Form5_result_value := 1;
    close;
  End;
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  Form5_result_value := 0;
  close;
End;

End.

